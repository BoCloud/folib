package com.veadan.folib.controllers.ahzw;


import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.util.RepositoryPathUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/artifact/folib/offline")
@Api(value = "/api/artifact/folib/offline")
@Slf4j
public class OfflineArtifactUploadController extends BaseArtifactController {

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Autowired
    private ConversionService conversionService;

    @Autowired
    private ClusterSyncService clusterSyncService;


    // 普通制品离线制品上传
    @PostMapping("/upload")
    @ApiOperation(value = "离线上传制品", notes = "离线上传制品")
    public ResponseEntity offlineUpload(@RequestParam("file") MultipartFile file,
                                        @RequestParam("storageId") String storageId,
                                        @RequestParam("repostoryId") String repostoryId,
                                        @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        // 离线普通制品folib 生成存储路径自动生成版本号 eg: /1/file
        try (InputStream is = file.getInputStream()) {
            // 获取版本号
            validateRepo(storageId, repostoryId);

            RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repostoryId, "");

            Integer version = getIncrementalVersion(artifactPath);

            RepositoryPath versionPath = repositoryPathResolver.
                    resolve(storageId, repostoryId, version + "/" + file.getOriginalFilename());

            artifactManagementService.store(versionPath, is);
        } catch (Exception e) {
            e.printStackTrace();
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "离线普通制品上传失败", e, accept);
        }
        return getSuccessfulResponseEntity("ok", accept);
    }

    private void validateRepo(String storageId, String repostoryId) throws Exception {
        if (null == repositoryManagementService.getStorage(storageId)) {
            throw new Exception("Storage [" + storageId + "] not exist!");
        }
        Repository repository = repositoryManagementService.getStorage(storageId).getRepository(repostoryId);
        if (null == repository) {
            RepositoryDto repositoryDto = new RepositoryDto(repostoryId);
            repositoryDto.setPolicy("mixed");
            repositoryDto.setStorageProvider("local");
            repositoryDto.setLayout("Raw");
            repositoryDto.setType("hosted");
            repositoryDto.setStatus("In Service");
            configurationManagementService.saveRepository(storageId, repositoryDto);
            RepositoryDto repoDto = getMutableConfigurationClone().getStorage(storageId)
                    .getRepository(repostoryId);

            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repositoryDto));
            if (!Files.exists(repositoryPath)) {
                repositoryManagementService.createRepository(storageId, repostoryId);
            }
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repoDto, storageId, repostoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
            clusterSyncService.syncRepository(syncRepositoryDto);

        }
    }

    private Integer getIncrementalVersion(RepositoryPath artifactPath) throws Exception {
        List<String> fileRelativePaths = RepositoryPathUtil.getFileRelativePaths(artifactPath);
        fileRelativePaths = fileRelativePaths.stream().filter(s ->
                        !s.endsWith(".md5") && !s.startsWith(".trash") && !s.endsWith(".sha1") && !s.endsWith(".sha256"))
                .collect(Collectors.toList());
        Integer version = 0;
        if (CollectionUtils.isEmpty(fileRelativePaths)) {
            return 1;
        }
        for (String filePath : fileRelativePaths) {
            String[] array = filePath.split(String.valueOf(File.separatorChar));
            if (array.length != 2) {
                continue;
            }
            Integer temp = Integer.parseInt(array[0]);
            if(temp>version){
                version=temp;
            }

        }
        return version + 1;
    }


}
