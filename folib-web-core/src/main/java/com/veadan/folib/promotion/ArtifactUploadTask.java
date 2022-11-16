package com.veadan.folib.promotion;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.Callable;

public class ArtifactUploadTask implements Callable<String> {

    private String storageId;
    private String repostoryId;
    private MultipartFile file;
    private RepositoryManagementService repositoryManagementService;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private String fileRelativePath;

    public ArtifactUploadTask() {
    }

    public ArtifactUploadTask(String storageId,
                              String repostoryId,
                              MultipartFile file,
                              RepositoryManagementService repositoryManagementService,
                              RepositoryPathResolver repositoryPathResolver,
                              ArtifactManagementService artifactManagementService, String fileRelativePath) {
        this.storageId = storageId;
        this.repostoryId = repostoryId;
        this.file = file;
        this.repositoryManagementService = repositoryManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.fileRelativePath = fileRelativePath;
    }

    @Override
    public String call() {
        String rs = "";
        try (InputStream is = file.getInputStream()) {
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repostoryId, fileRelativePath);
            artifactManagementService.store(destPath, is);
        } catch (IOException e) {
            e.printStackTrace();
            rs = e.getMessage();
        }
        return rs;
    }
}
