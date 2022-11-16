package com.veadan.folib.promotion;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.Data;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.Callable;

@Data
public class ArtifactDoStoreTask implements Callable<String> {

    private InputStream fileIs;
    private String path;
    private String storageId;
    private String repostoryId;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;

    public ArtifactDoStoreTask(InputStream fileIs, String path, String storageId, String repostoryId, RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService) {
        this.fileIs = fileIs;
        this.path = path;
        this.storageId = storageId;
        this.repostoryId = repostoryId;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
    }

    @Override
    public String call() {
        String rs = "";
        try (InputStream is = getFileIs()) {
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repostoryId, path);
            artifactManagementService.store(destPath, is);
        } catch (IOException e) {
            e.printStackTrace();
            rs = e.getMessage();
        }
        return rs;
    }
}
