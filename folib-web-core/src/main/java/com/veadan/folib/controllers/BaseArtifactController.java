package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.ArtifactControllerHelper;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.util.Objects;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private HttpServletResponse httpServletResponse;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Autowired
    private ArtifactComponent artifactComponent;

    @Autowired
    private ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor;


    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception {

        // If the response is already committed, there's no need to proceed.
        if (response.isCommitted()) {
            return false;
        }

        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath);

        // If the resource is not found, return false.
        if (response.getStatus() == HttpStatus.NOT_FOUND.value()) {
            return false;
        }

        // If it's a HEAD request, return true.
        if (RequestMethod.HEAD.name().equals(request.getMethod())) {
            return true;
        }
        long startTime = System.currentTimeMillis();
        logger.debug("Download {} 开始时间 {}", repositoryPath.toString(), startTime);
        artifactComponent.beforeRead(repositoryPath);

        try (FileChannel fileChannel = FileChannel.open(repositoryPath)) {
            WritableByteChannel responseChannel = Channels.newChannel(response.getOutputStream());
            long fileSize = fileChannel.size();

            // Handle partial content using ranges from httpHeaders and adjust the start and end accordingly.
            if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
                // adjust based on range from headers
                long start = 0;
                // adjust based on range from headers
                long end = fileSize - 1;
                long length = end - start + 1;

                response.setStatus(HttpStatus.PARTIAL_CONTENT.value());
                // Set appropriate Content-Range header values.
                response.setHeader("Content-Range", "bytes " + start + "-" + end + "/" + fileSize);

                fileChannel.transferTo(start, length, responseChannel);
                artifactComponent.afterRead(repositoryPath);
            } else {
                fileChannel.transferTo(0, fileSize, responseChannel);
                artifactComponent.afterRead(repositoryPath);
            }
        }
        logger.debug("Download {} 结束时间 {}", repositoryPath.toString(), System.currentTimeMillis() - startTime);
        return true;
    }

    public ResponseEntity<String> checkRepositoryAccess() {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

    /**
     * 漏洞阻断下载
     *
     * @param repositoryPath 制品信息
     * @throws IOException io异常
     */
    public Artifact vulnerabilityBlock(RepositoryPath repositoryPath) throws IOException {
        boolean supportLayout = artifactComponent.layoutSupportsForBlock(repositoryPath);
        if (!supportLayout) {
            return null;
        }
        String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + "-metadata", artifactStr = "";
        RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
        Artifact artifact = null;
        long startTime = System.currentTimeMillis();
        logger.info("Block JSON {} 开始时间 {}", repositoryPath.toString(), startTime);
        if (Files.exists(artifactRepositoryPath)) {
            artifactStr = Files.readString(artifactRepositoryPath);
            if (StringUtils.isNotBlank(artifactStr)) {
                artifact = JSON.parseObject(artifactStr, Artifact.class);
            }
        }
        logger.info("Block JSON {} 结束时间 {}", repositoryPath.toString(), System.currentTimeMillis() - startTime);
        if (Objects.isNull(artifact)) {
            artifact = repositoryPath.getArtifactEntry();
            if (Objects.isNull(artifact)) {
                return null;
            }
            Files.writeString(artifactRepositoryPath, JSONObject.toJSONString(artifact));
        }
        boolean block = artifactComponent.vulnerabilityBlock(artifact, repositoryPath.getRepository().getLayout());
        if (block) {
            httpServletResponse.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
            httpServletResponse.setStatus(HttpServletResponse.SC_FORBIDDEN);
            String msg = "The artifact " + artifact.getUuid() + " has a vulnerability, and downloading is prohibited";
            httpServletResponse.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(msg)));
            httpServletResponse.flushBuffer();
            artifactEventListenerRegistry.dispatchArtifactDownloadBlockedEvent(repositoryPath);
        }
        return artifact;
    }

    protected String getBaseUrl() {
        return StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
    }

    protected String getBaseUrl(Repository repository) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    public boolean artifactRealExists(RepositoryPath repositoryPath) {
        try {
            return Objects.nonNull(repositoryPath) && Files.exists(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry()) && Boolean.TRUE.equals(repositoryPath.getArtifactEntry().getArtifactFileExists());
        } catch (Exception ex) {
            logger.error("判断制品是否存在发生错误：{}", ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }
}
