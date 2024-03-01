package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.veadan.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.veadan.folib.enums.JFrogEventTypeEnum;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.schema2.ContainerConfigurationManifest;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.security.exceptions.ExpiredTokenException;
import com.veadan.folib.security.exceptions.InvalidTokenException;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.users.security.SecurityTokenProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2024/2/26
 **/
@Slf4j
@RequestMapping("/artifactory")
@RestController
public class ArtifactWebHookController {

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected DockerComponent dockerComponent;

    @PostMapping("/webhook")
    public ResponseEntity<Object> webhook(@RequestBody String data, HttpServletRequest request) {
        log.info("JFrog event data [{}]", data);
        try {
            WebhookDto webhookDto = JSONObject.parseObject(data, WebhookDto.class);
            String tokenKey = "X-jfrog-event-auth";
            String token = request.getHeader(tokenKey);
            log.info("JFrog event header token [{}] [{}]", tokenKey, token);
            if (StringUtils.isBlank(token)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The header parameter [%s] is required", tokenKey));
            }
            securityTokenProvider.getClaims(token, true);
            if (!JFrogEventTypeEnum.needHandle(webhookDto.getEventType())) {
                log.info("JFrog event [{}] not need handle", webhookDto.getEventType());
                return ResponseEntity.ok("");
            }
            String repositoryHeaderKey = "X-repository";
            String repositoryHeader = request.getHeader(repositoryHeaderKey);
            log.info("JFrog event header repository [{}] [{}]", repositoryHeaderKey, repositoryHeader);
            if (StringUtils.isBlank(repositoryHeader)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The header parameter [%s] is required", repositoryHeaderKey));
            }
            String storageId = ConfigurationUtils.getStorageId(repositoryHeader, repositoryHeader);
            String repositoryId = ConfigurationUtils.getRepositoryId(repositoryHeader);
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                log.warn("JFrog event storage [{}] not found", storageId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                log.warn("JFrog event repository [{}] not found", repositoryId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
            }
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            ArtifactData artifactData = webhookDto.getData();
            RepositoryPath repositoryPath = rootRepositoryPath.resolve(artifactData.getPath());
            boolean exists = false;
            String currentDigest = "";
            if (StringUtils.isNotBlank(artifactData.getSha256()) && Files.exists(repositoryPath)) {
                LayoutFileSystemProvider provider = rootRepositoryPath.getFileSystem().provider();
                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, MessageDigestAlgorithms.SHA_256);
                if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                    try {
                        currentDigest = Files.readString(checksumPath);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    exists = artifactData.getSha256().equals(currentDigest);
                }
            }
            if (exists) {
                log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] exists skip sync", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
                return ResponseEntity.ok("");
            }
            log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] not exists", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
            if (ProductTypeEnum.Docker.getName().equals(webhookDto.getDomain())) {
                //docker
                String path = artifactData.getPath();
                String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(artifactData.getTag())), GlobalConstants.SEPARATOR);
                RemoteRepository remoteRepository = repositoryPath.getRepository().getRemoteRepository();
                String remoteUrl = StringUtils.removeEnd(remoteRepository.getUrl(), GlobalConstants.SEPARATOR);
                String digestOrTag = "";
                boolean isTag = false;
                if (!artifactData.getTag().startsWith(GlobalConstants.SHA_256)) {
                    digestOrTag = artifactData.getTag();
                    isTag = true;
                } else {
                    digestOrTag = artifactData.getTag().replace("__", ":");
                }
                if (!remoteUrl.endsWith(GlobalConstants.DOCKER_V2) || imagePath.split(GlobalConstants.SEPARATOR).length > 1) {
                    imagePath = imagePath.replace(GlobalConstants.DOCKER_DEFAULT_REPO.concat(GlobalConstants.SEPARATOR), "");
                }
                RepositoryPath manifestRepositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, imagePath, digestOrTag);
                if (isTag && Objects.nonNull(manifestRepositoryPath)) {
                    List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(manifestRepositoryPath);
                    if (CollectionUtils.isNotEmpty(imageManifestList)) {
                        RepositoryPath blobsRepositoryPath;
                        for (ImageManifest imageManifest : imageManifestList) {
                            ContainerConfigurationManifest containerConfigurationManifest = imageManifest.getConfig();
                            if (Objects.nonNull(containerConfigurationManifest) && StringUtils.isNotBlank(containerConfigurationManifest.getDigest())) {
                                blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + containerConfigurationManifest.getDigest());
                                String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(imagePath, "/"), containerConfigurationManifest.getDigest());
                                blobsRepositoryPath.setTargetUrl(targetUrl);
                                blobsRepositoryPath.setArtifactPath(imagePath);
                                artifactResolutionService.resolvePath(blobsRepositoryPath);
                            }
                            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                                for (LayerManifest layerManifest : imageManifest.getLayers()) {
                                    if (StringUtils.isNotBlank(layerManifest.getDigest())) {
                                        blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + layerManifest.getDigest());
                                        String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(imagePath, "/"), layerManifest.getDigest());
                                        blobsRepositoryPath.setTargetUrl(targetUrl);
                                        blobsRepositoryPath.setArtifactPath(imagePath);
                                        artifactResolutionService.resolvePath(blobsRepositoryPath);
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                artifactResolutionService.resolvePath(repositoryPath);
            }
            return ResponseEntity.ok("");
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            if (ex instanceof ExpiredTokenException) {
                log.warn("JFrog event the token has expired");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token has expired");
            } else if (ex instanceof InvalidTokenException) {
                log.warn("JFrog event the token is invalid");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token is invalid");
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(String.format("Handle event error [%s]", data));
        }
    }
}
