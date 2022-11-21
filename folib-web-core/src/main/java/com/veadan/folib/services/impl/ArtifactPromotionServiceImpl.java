package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.*;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.FutureTask;

/**
 * @author qijianping
 */
@Service
@Slf4j
public class ArtifactPromotionServiceImpl implements ArtifactPromotionService {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";
    private final String pullURI = "/api/artifact/folib/promotion/pull-files";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Autowired
    private PromotionUtil promotionUtil;

    @Autowired
    private ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Value("${folib.host:localhost}")
    private String host;

    @Value("${folib.port}")
    private int port;


    @Override
    public ResponseEntity copy(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            final String srcStorageId = artifactPromotion.getSrcStorageId();
            final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
            Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);

            // 多个目标仓库复制
            artifactPromotion.getTargetRepositoyList().forEach(x -> {
                String destStorageId = x.getTargetStorageId();
                String destRepositoryId = x.getTargetRepositoryId();
                log.info("Copying {} from {}:{} to {}:{}...", artifactPromotion.getPath(), srcStorageId, srcRepositoryId, destStorageId,
                        destRepositoryId);
                singleCopy(artifactPromotion, srcRepository, destStorageId, destRepositoryId);
            });
        } catch (Exception e) {
            log.error("Unable to copy artifact", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact copying");
    }

    private void checkParam(ArtifactPromotion artifactPromotion) throws Exception {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();

        if (null == repositoryManagementService.getStorage(srcStorageId)) {
            throw new Exception("The source StorageId does not exist!");
        }

        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        if (null == srcRepository) {
            throw new Exception("The source RepositoryId does not exist!");
        }

        if (!srcRepository.getType().equals("hosted")) {
            throw new Exception("The source RepositoryId does not local");
        }

        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        if (!Files.exists(srcRepositoryPath)) {
            throw new Exception("The source path does not exist!");
        }
        List<TargetRepositoyDto> targetList = artifactPromotion.getTargetRepositoyList();

        if (CollectionUtils.isEmpty(targetList)) {
            throw new Exception("The target is empty");
        }
        StringBuilder stringBuilder = new StringBuilder();
        for (TargetRepositoyDto dto : targetList) {
            String targetStorageId = dto.getTargetStorageId();
            String targetRepositoryId = dto.getTargetRepositoryId();
            if (null == repositoryManagementService.getStorage(targetStorageId)) {
                stringBuilder.append("Storage : ").append(targetStorageId).append(" not exits");
                continue;
            }
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            if (null == targetRepository) {
                stringBuilder.append(System.lineSeparator()).append(" Repository : ").append(targetRepositoryId).append(" not exits");
                continue;
            }
            if (!targetRepository.getType().equals("hosted")) {
                stringBuilder.append(System.lineSeparator()).append("Repository : ").append(targetRepositoryId).append("does not local");
            }
        }
        if (StringUtils.isNotBlank(stringBuilder.toString())) {
            throw new Exception(stringBuilder.toString());
        }
    }

    private void singleCopy(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        promotionUtil.executeHanleCopy(srcPath.getTarget().toString(), destRepository, srcRepository);
    }

    @Override
    public ResponseEntity move(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            promotionUtil.executeHandleMove(artifactPromotion);
        } catch (Exception e) {
            log.error("Unable to move artifact", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact moving");
    }

    @Override
    public ResponseEntity nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request) {
        try {
            String sourcePath = promotionNodeOption.getSourcePath();
            String targetPath = promotionNodeOption.getTargetPath();
            String srcStorageId = sourcePath.split(":")[2].split("/")[1];
            String srcRepostoryId = sourcePath.split(":")[2].split("/")[2];
            String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            String targetStorageId = targetPath.split(":")[2].split("/")[1];
            String targetRepostoryId = targetPath.split(":")[2].split("/")[2];
            String targetUrl = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[0];
            String targetUri = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[1];

            // 判断节点参数是 做推 push  或者 拉取 pull
            String requestURL = request.getRequestURL().toString().replace(request.getRequestURI(), "");
            if (sourcePath.contains(requestURL)) {
                // 本地源 制品路径 推向 目标路径
                Storage srcStorage = repositoryManagementService.getStorage(srcStorageId);// todo validate
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepostoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, srcUri);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepostoryId,
                        targetStorageId, targetRepostoryId, srcAbsolutePath, targetUrl + upLoadURI);

                PromotionNodeOptionDto uploadDto = promotionUtil.getPromotionUploadDto(promotionArtifactDto);

                //向目标仓库推包
                promotionUtil.upload(targetUrl + upLoadURI, uploadDto);

            } else if (targetPath.contains(requestURL)) {
                // 从源仓路径 pull 到目标仓路径
                Client client = clientPool.getRestClient();
                WebTarget target = client.target(srcUrl + pullURI);
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepostoryId,
                        targetStorageId, targetRepostoryId, targetUri, targetUrl + upLoadURI);
                Response response = target.request().post(Entity.entity(promotionArtifactDto, MediaType.APPLICATION_JSON));
                if (response.getStatus() > 210) {
                    log.error("Push artifact error {}", srcUrl);
                    throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
                }
                // 向目标仓库拉取后存入本地 存入 targetStorageId  targetRepostoryId
            }
        } catch (Exception e) {
            log.error("制品晋级错误 {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity upload(MultipartFile[] files, String storageId, String repostoryId, String filePathMap) {
        List<FutureTask<String>> listTask = new ArrayList<>();
        Map<String, String> mapType = JSON.parseObject(filePathMap, Map.class);
        for (MultipartFile file : files) {
            String fileRelativePath = mapType.get(file.getOriginalFilename());
            ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repostoryId, file,
                    repositoryManagementService, repositoryPathResolver, artifactManagementService, fileRelativePath);
            FutureTask<String> task = new FutureTask<String>(artifactUploadTask);
            listTask.add(task);
            asyncRepositoryThreadPoolExecutor.submit(task);
        }
        StringBuilder temp = new StringBuilder();
        for (FutureTask<String> task : listTask) {
            try {
                String resultMsg = task.get();
                if (StringUtils.isNotBlank(resultMsg)) {
                    temp.append(resultMsg).append(System.lineSeparator());
                    log.error(resultMsg);
                }

            } catch (Exception e) {
                temp.append(e.getMessage()).append(System.lineSeparator());
                log.error("upload exception {}", e.getMessage());
            }
        }
        if (StringUtils.isNotBlank(temp.toString())) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(temp.toString());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactDto.getStorageId(),
                artifactDto.getRepostoryId(),
                artifactDto.getPath());
        try (InputStream in = Files.newInputStream(repositoryPath);
        ) {
            OutputStream out = response.getOutputStream();
            response.setCharacterEncoding("UTF-8");
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + " filename=" + repositoryPath.getFileName().toString());
            int byteRead = 0;
            byte[] buffer = new byte[512];
            while ((byteRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, byteRead);
            }
            out.flush();
        } catch (IOException e) {
            log.error("download exception {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity pull(PromotionArtifactDto promotionArtifactDto) {
        try {
            String storageId = promotionArtifactDto.getSrcStorageId();
            String repostoryId = promotionArtifactDto.getSrcRepostoryId();
            String path = promotionArtifactDto.getPath();
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(storageId, repostoryId, path);
            promotionArtifactDto.setPath(targetRepositoryPath.getTarget().toString());
            PromotionNodeOptionDto promotionNodeOptionDto = promotionUtil.getPromotionPullDto(promotionArtifactDto);

            // 向目标仓库传
            promotionUtil.upload(promotionArtifactDto.getUploadHost(), promotionNodeOptionDto);
        } catch (Exception e) {
            log.error("pull exception {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }
}
