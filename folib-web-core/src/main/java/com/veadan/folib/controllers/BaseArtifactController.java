package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilitiesInfo;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.utils.ArtifactControllerHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Value("${folib.dependentPushUrl}")
    private String pushUrl;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private HttpServletResponse httpServletResponse;

    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Autowired
    private ArtifactComponent artifactComponent;


    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception {
        logger.debug("Resolved path: {}", repositoryPath);
        boolean isCommitted = response.isCommitted();
        if (isCommitted) {
            return false;
        }
        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath);
        if (response.getStatus() == HttpStatus.NOT_FOUND.value()) {
            return false;
        } else if (request.getMethod().equals(RequestMethod.HEAD.name())) {
            return true;
        }


        try (InputStream is = artifactResolutionService.getInputStream(repositoryPath)) {
            if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
                logger.debug("Detected ranged request.");

                ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            } else {
                copyToResponse(is, response);
            }
        }

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
    public void vulnerabilityBlock(RepositoryPath repositoryPath) throws IOException {
        boolean supportLayout = artifactComponent.layoutSupportsForBlock(repositoryPath);
        if (!supportLayout) {
            return;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (Objects.nonNull(artifact)) {
            boolean block = artifactComponent.vulnerabilityBlock(artifact);
            if (block) {
                httpServletResponse.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
                httpServletResponse.setStatus(HttpServletResponse.SC_FORBIDDEN);
                String msg = "The artifact " + artifact.getUuid() + " has a vulnerability, and downloading is prohibited";
                httpServletResponse.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(msg)));
                httpServletResponse.flushBuffer();
                artifactEventListenerRegistry.dispatchArtifactDownloadBlockedEvent(repositoryPath);
                //推数据给platform
                pushVulnerabilities(artifact);
            }
        }
    }

    private void pushVulnerabilities(Artifact artifact) {
        Response response = null;
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HH:mm:ss");
            String id = LocalDateTime.now().format(formatter) + "-" + UUID.randomUUID().toString();
            String bugName = JSON.toJSONString(artifact.getVulnerabilitySet().stream().
                    map(Vulnerability::getUuid).distinct().collect(Collectors.toList())
                    .get(0)).replace("\"", "");
            String repairVersion = JSON.toJSONString(artifact.getVulnerabilitySet().stream().
                    map(Vulnerability::getVersionEndExcluding).collect(Collectors.toList())
                    .get(0)).replace("\"", "");
            String packagePath = artifact.getArtifactPath();
            String[] array = packagePath.split("/");
            String packageName = array[array.length - 1];
            VulnerabilitiesInfo vulnerabilitiesInfo =
                    VulnerabilitiesInfo.builder()
                            .id(id)
                            .appId(artifact.getStorageId())
                            .storageId(artifact.getStorageId())
                            .repositoryId(artifact.getRepositoryId())
                            .bugName(bugName)
                            .packageName(packageName)
                            .packagePath(packagePath)
                            .repairVersion(repairVersion)
                            .report(artifact.getReport()).build();
            Client client = clientPool.getRestClient();
            String url = pushUrl + "/devopsplatform/apis/v1/folib/pushVulnerabilities";
            WebTarget target = client.target(url);
            response = target.request().post(Entity.entity(vulnerabilitiesInfo, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                throw new Exception("{} get error" + url);
            }
            logger.info("已成功推送漏洞阻断数据");
        } catch (Exception e) {
            logger.error("依赖库漏洞阻断推数据失败");
            e.printStackTrace();
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

}
