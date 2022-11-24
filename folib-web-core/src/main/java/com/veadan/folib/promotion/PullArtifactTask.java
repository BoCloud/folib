package com.veadan.folib.promotion;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.InputStream;
import java.util.concurrent.Callable;

@Slf4j
public class PullArtifactTask implements Callable<String> {
    private String path;
    private String fileUlr;
    private String targetStorageId;
    private String targetRepostoryId;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    public PullArtifactTask(String path, String fileUlr, String targetStorageId,
                            String targetRepostoryId, RepositoryPathResolver repositoryPathResolver,
                            ArtifactManagementService artifactManagementService,
                            ProxyRepositoryConnectionPoolConfigurationService clientPool) {
        this.path = path;
        this.fileUlr = fileUlr;
        this.targetStorageId = targetStorageId;
        this.targetRepostoryId = targetRepostoryId;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.clientPool = clientPool;
    }

    @Override
    public String call() throws Exception {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(fileUlr);
        Response response = target.request().get();
        RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
        try (InputStream is = response.readEntity(InputStream.class);) {
            artifactManagementService.store(destPath, is);
        }
        log.info("File {} pulled", fileUlr);
        return "ok";
    }
}
