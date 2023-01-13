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
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(srcUrl);
        Response response = target.request().post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
        RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
        try (InputStream is = response.readEntity(InputStream.class);) {
            artifactManagementService.store(destPath, is);
            promotionUtil.setMetaData(destPath, metaData);
        }
        log.info("File {} pulled", JSON.toJSONString(artifac));
        return "ok";
    }
}
