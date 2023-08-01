package com.veadan.folib.controllers.ahzw;


import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.impl.FqlSearchService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.search.SearchResults;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import java.io.InputStream;
import java.nio.file.Files;

@RestController
@RequestMapping("/api/artifact/folib/offline")
@Api(value = "离线制品管理",tags = "离线制品管理")
@Slf4j
public class OfflineArtifactUploadController extends BaseArtifactController {

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Autowired
    private ClusterSyncService clusterSyncService;

    @Inject
    private ArtifactWebService artifactWebService;

    @Inject
    private FqlSearchService fqlSearchService;


    // 普通制品离线制品上传
    @PostMapping("/upload")
    @ApiOperation(value = "离线上传制品", notes = "离线上传制品")
    public ResponseEntity offlineUpload(@RequestParam("file") MultipartFile file,
                                        @RequestParam("storageId") String storageId,
                                        @RequestParam("repostoryId") String repostoryId,
                                        @RequestParam("deployType") String deployType,
                                        @RequestParam(value = "packageVersionDesc",required = false) String packageVersionDesc,
                                        @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        // 离线普通制品folib 生成存储路径自动生成版本号 eg: repoName/1/file
        try (InputStream is = file.getInputStream()) {
            // 获取版本号
            validateRepo(storageId, repostoryId);

            RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repostoryId, repostoryId);

            Long version = getIncrementalVersion(artifactPath);

            RepositoryPath versionPath = repositoryPathResolver.
                    resolve(storageId, repostoryId, repostoryId + "/" + version + "/" + file.getOriginalFilename());

            artifactManagementService.store(versionPath, is);

            // 添加部署类型
            ArtifactMetadataForm deployTypeMetadataForm = ArtifactMetadataForm.builder()
                    .viewShow(1).key("deployType").value(deployType)
                    .repositoryId(repostoryId).type("STRING").storageId(storageId)
                    .artifactPath(repostoryId + "/" + version + "/" + file.getOriginalFilename()).build();

            artifactWebService.saveArtifactMetadata(deployTypeMetadataForm);


            // 添加入库方式的元数据信息
            ArtifactMetadataForm srcTypeMetadataForm = ArtifactMetadataForm.builder()
                    .viewShow(1).key("srcType").value("manual")
                    .repositoryId(repostoryId).type("STRING").storageId(storageId)
                    .artifactPath(repostoryId + "/" + version + "/" + file.getOriginalFilename()).build();
            artifactWebService.saveArtifactMetadata(srcTypeMetadataForm);

            // 添加制品版本描述
            if (StringUtils.isNotBlank(packageVersionDesc)) {
                ArtifactMetadataForm descMetadataForm = ArtifactMetadataForm.builder()
                        .viewShow(1).key("packageVersionDesc").value(packageVersionDesc)
                        .repositoryId(repostoryId).type("STRING").storageId(storageId)
                        .artifactPath(repostoryId + "/" + version + "/" + file.getOriginalFilename()).build();
                artifactWebService.saveArtifactMetadata(descMetadataForm);
            }
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
            repositoryDto.setArtifactMaxSize(214748364800L);
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

    private Long getIncrementalVersion(RepositoryPath artifactPath) throws Exception {
        // docker 布局的版本总数+ raw 版本总数
        String rawRepo = artifactPath.getRepositoryId();
        String dockerRepo = rawRepo.replace("-raw", "");
        String storageId = artifactPath.getStorageId();

        // 搜索 docker  raw 仓总的制品数
        String regex = "(%s)(.*%s.*)";
        String prefix = storageId;
        if (StringUtils.isNotBlank(dockerRepo)) {
            prefix = prefix + "-" + dockerRepo;
        }
        regex = String.format(regex, prefix, "-");
        SearchResults dockerResult = fqlSearchService.artifactQuery(true,
                regex, null, storageId,
                dockerRepo, null,null, null, null, null, null, null, Integer.MAX_VALUE, 1);
        SearchResults rawResult = fqlSearchService.artifactQuery(false,
                storageId + "-" + dockerRepo, null, storageId,
                rawRepo, null,null, null, null, null, null, null, Integer.MAX_VALUE, 1);
        return dockerResult.getTotal() + rawResult.getTotal() + 1;


//        List<String> fileRelativePaths = RepositoryPathUtil.getFileRelativePaths(artifactPath);
//        fileRelativePaths = fileRelativePaths.stream().filter(s ->
//                        !s.endsWith(".md5") && !s.startsWith(".trash") && !s.endsWith(".sha1") && !s.endsWith(".sha256"))
//                .collect(Collectors.toList());
//        Integer version = 0;
//
//        if(null== dockerRepoImagePath&&CollectionUtils.isEmpty(fileRelativePaths)){
//            return 1;
//        }
//
//        if (CollectionUtils.isEmpty(fileRelativePaths)) {
//            return 1;
//        }
//        for (String filePath : fileRelativePaths) {
//            String[] array = filePath.split(String.valueOf(File.separatorChar));
//            if (array.length != 3) {
//                continue;
//            }
//            Integer temp = Integer.parseInt(array[1]);
//            if(temp>version){
//                version=temp;
//            }
//
//        }
//        return version + 1;
    }


}
