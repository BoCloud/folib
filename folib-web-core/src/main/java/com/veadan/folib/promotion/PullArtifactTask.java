package com.veadan.folib.promotion;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.InputStream;
import java.nio.file.Files;
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
    }

    @Override
    public String call() throws Exception {
        try {
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Response response = target.request().post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !path.endsWith(".sha256")) {
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
        }
        log.info("File {} pulled", JSON.toJSONString(artifac));
        return "ok";
    }

    private boolean reTryPull() {
        try {
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Response response = target.request().post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !path.endsWith(".sha256")) {
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
        }
    }
}
