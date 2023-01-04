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
    private String metaData;
    private PromotionUtil promotionUtil;

    public ArtifactUploadTask() {
    }

    public ArtifactUploadTask(String storageId,
                              String repostoryId,
                              MultipartFile file,
                              RepositoryManagementService repositoryManagementService,
                              RepositoryPathResolver repositoryPathResolver,
                              ArtifactManagementService artifactManagementService,
                              PromotionUtil promotionUtil,
                              String fileRelativePath,String metaData ) {
        this.storageId = storageId;
        this.repostoryId = repostoryId;
        this.file = file;
        this.repositoryManagementService = repositoryManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.promotionUtil = promotionUtil;
        this.fileRelativePath = fileRelativePath;
        this.metaData = metaData;
    }

    @Override
    public String call() {
        String rs = "";
        try (InputStream is = file.getInputStream()) {
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repostoryId, fileRelativePath);
//            String layout = destPath.getRepository().getLayout();
//            String type = destPath.getRepository().getType();
//            if (!"Raw".equalsIgnoreCase(layout) && !"hosted".equals(type)) { // 暂时只支持Raw 布局的上传
//                throw new IOException(fileRelativePath + "非Raw布局的本地仓库制品不可上传!");
//            }
            artifactManagementService.store(destPath, is);
            promotionUtil.setMetaData(destPath,metaData);
        } catch (IOException e) {
            e.printStackTrace();
            rs = e.getMessage();
        }
        return rs;
    }
}
