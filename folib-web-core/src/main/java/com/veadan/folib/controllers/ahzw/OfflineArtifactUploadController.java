package com.veadan.folib.controllers.ahzw;


import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.RepositoryPathUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/artifact/folib/offline")
@Api(value = "/api/artifact/folib/offline")
@Slf4j
public class OfflineArtifactUploadController extends BaseArtifactController {


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
            RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repostoryId, repostoryId);

            Integer version = getIncrementalVersion(artifactPath);

            RepositoryPath versionPath = repositoryPathResolver.
                    resolve(storageId, repostoryId, repostoryId + "/" + version);

            artifactManagementService.store(versionPath, is);
        } catch (Exception e) {
            e.printStackTrace();
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "离线普通制品上传失败", e, accept);
        }
        return getSuccessfulResponseEntity("ok", accept);
    }

    private Integer getIncrementalVersion(RepositoryPath artifactPath) throws Exception {
        List<String> fileRelativePaths = RepositoryPathUtil.getFileRelativePaths(artifactPath);
        fileRelativePaths = fileRelativePaths.stream().filter(s ->
                        !s.endsWith(".md5") && !s.startsWith(".trash") && !s.endsWith(".sha1") && !s.endsWith(".sha256"))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(fileRelativePaths)) {
            return 1;
        }
        return fileRelativePaths.size() + 1;
    }


}
