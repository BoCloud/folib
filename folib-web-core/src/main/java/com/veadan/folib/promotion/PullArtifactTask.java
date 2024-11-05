package com.veadan.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Objects;
import java.util.concurrent.Callable;

@Slf4j
public class PullArtifactTask implements Callable<String> {

    private String path;
    private String srcUrl;
    private String targetStorageId;
    private String targetRepostoryId;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;
    private ArtifactDto artifac;
    private String metaData;
    private PromotionUtil promotionUtil;
    private SecurityComponent securityComponent;

    public PullArtifactTask(String path, String srcUrl, String targetStorageId, String targetRepostoryId,
                            RepositoryPathResolver repositoryPathResolver,
                            ArtifactManagementService artifactManagementService,
                            ProxyRepositoryConnectionPoolConfigurationService clientPool,
                            PromotionUtil promotionUtil,
                            ArtifactDto artifac,
                            String metaData) {
        this.path = path;
        this.srcUrl = srcUrl;
        this.targetStorageId = targetStorageId;
        this.targetRepostoryId = targetRepostoryId;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.clientPool = clientPool;
        this.artifac = artifac;
        this.metaData = metaData;
        this.promotionUtil = promotionUtil;
        this.securityComponent = SpringUtil.getBean(SecurityComponent.class);
    }

    @Override
    public String call() throws Exception {
        Response response = null;
        try {
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            if (RepositoryFiles.isChecksum(destPath)) {
                return "ok";
            }
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !DockerArtifactCoordinates.exclude(path)) {
                    try (InputStream is = response.readEntity(InputStream.class);) {
                        Files.copy(is, destPath);
                    }
                    return "ok";
                }
            }
            try (InputStream is = response.readEntity(InputStream.class);) {
                promotionUtil.setMetaData(destPath, metaData);
                artifactManagementService.store(destPath, is);
            }
        } catch (Exception e) {
            // 添加重试机制
            log.error("{} pull error {}", JSON.toJSONString(artifac), e.getMessage());
            boolean rePullResultFlag = false;
            for (int i = 0; i < 5; i++) {
                rePullResultFlag = reTryPull();
                if (rePullResultFlag) {
                    break;
                }
                Thread.sleep(1000L);
            }
            if (!rePullResultFlag) {
                throw new Exception(e.getMessage());
            }
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        log.info("File {} pulled", JSON.toJSONString(artifac));
        return "ok";
    }

    private boolean reTryPull() {
        Response response = null;
        try {
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !DockerArtifactCoordinates.exclude(path)) {
                    try (InputStream is = response.readEntity(InputStream.class);) {
                        Files.copy(is, destPath);
                    }
                    return true;
                }
            }

            try (InputStream is = response.readEntity(InputStream.class);) {
                promotionUtil.setMetaData(destPath, metaData);
                artifactManagementService.store(destPath, is);
            }
            return true;
        } catch (Exception e) {
            return false;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }
}
