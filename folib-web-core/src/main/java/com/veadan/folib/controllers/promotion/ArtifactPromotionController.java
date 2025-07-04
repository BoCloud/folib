package com.veadan.folib.controllers.promotion;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.model.request.ArtifactSliceUploadWebReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceUploadInfoRes;
import com.veadan.folib.model.response.Result;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;

import java.util.*;

/**
 * 制品晋级控制层
 *
 * @author veadan
 */
@RestController
@RequestMapping("/api/artifact/folib/promotion")
@Api(description = "制品文件管理", tags = "制品文件管理")
@Slf4j
public class ArtifactPromotionController extends BaseArtifactController {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @Inject
    private SecurityComponent securityComponent;

    @Value("${folib.temp}")
    private String tempPath;

    @PostMapping("/sync/copy")
    @PermissionCheck(resourceKey = "ARTIFACTS_COPY", storageKey = "srcStorageId", repositoryKey = "srcRepositoryId")
    public ResponseEntity syncCopy(@RequestBody @Validated ArtifactPromotion artifactPromotion,
                               BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.syncCopy(artifactPromotion);
    }

    @PostMapping("/sync/move")
    @PermissionCheck(resourceKey = "ARTIFACTS_MOVE", storageKey = "srcStorageId", repositoryKey = "srcRepositoryId")
    public ResponseEntity syncMove(@RequestBody @Validated ArtifactPromotion artifactPromotion, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.syncMove(artifactPromotion);
    }
    @PostMapping("/copy")
    @PermissionCheck(resourceKey = "ARTIFACTS_COPY", storageKey = "srcStorageId", repositoryKey = "srcRepositoryId")
    public ResponseEntity copy(@RequestBody @Validated ArtifactPromotion artifactPromotion,
                               BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.copy(artifactPromotion);
    }

    @PostMapping("/move")
    @PermissionCheck(resourceKey = "ARTIFACTS_MOVE", storageKey = "srcStorageId", repositoryKey = "srcRepositoryId")
    public ResponseEntity move(@RequestBody @Validated ArtifactPromotion artifactPromotion, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.move(artifactPromotion);
    }

    @PostMapping(value = "/upload-files")
    @ApiOperation(value = "文件上传(支持批量)", notes = "文件上传(支持批量)")
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repostoryId")
    public ResponseEntity upload(@RequestParam("files") MultipartFile[] files,
                                 @RequestParam("storageId") String storageId,
                                 @RequestParam("repostoryId") String repositoryId,
                                 @RequestParam("filePathMap") String filePathMap,
                                 @RequestParam(name = "fileMetaDataMap", required = false) String fileMetaDataMap,
                                 @RequestParam(name = "uuid", required = false) String uuid,
                                 @RequestParam(name = "imageTag", required = false) String imageTag,
                                 @RequestParam(name = "fileType", required = false) String fileType) {
        String baseUrl = getBaseUrl();
        String token = securityComponent.getSecurityToken();

        return artifactPromotionService.upload(files, storageId, repositoryId, filePathMap, fileMetaDataMap, uuid, imageTag, fileType, baseUrl, token);
    }

    @PostMapping(value = "/upload")
    @ApiOperation(value = "文件上传", notes = "文件上传")
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repositoryId")
    public ResponseEntity upload(
            @RequestParam("storageId") String storageId,
            @RequestParam("repositoryId") String repositoryId,
            @RequestParam(name = "parseArtifact") String parseArtifact) {
        return artifactPromotionService.upload(parseArtifact, storageId, repositoryId);
    }

    @PostMapping(value = "/download")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity download(@RequestBody @Validated ArtifactDto artifactDto,
                                   HttpServletResponse response, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.download(artifactDto, response);
    }

    @PostMapping(value = "/getFileRelativePaths")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity getFiles(@RequestBody @Validated ArtifactDto artifactDto,
                                   BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.getFileRelativePaths(artifactDto);
    }

    @PostMapping("/parseArtifact")
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repositoryId")
    public ResponseEntity<ArtifactParse> parseArtifact(@RequestParam("storageId") String storageId,
                                                       @RequestParam("repositoryId") String repositoryId, @RequestParam("file") MultipartFile file) {
        return ResponseEntity.ok(artifactPromotionService.parseArtifact(storageId, repositoryId, file));
    }

    /**
     * 文件上传进度
     *
     * @param dictType dictType
     * @param uuid     uuid
     */
    @GetMapping(value = "/uploadProcess")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    public ResponseEntity<List<Dict>> queryUploadProcess(@RequestParam("dictType") String dictType, @RequestParam(name = "uuid", required = false) String uuid) {
        return ResponseEntity.ok(artifactPromotionService.queryUploadProcess(dictType, uuid));
    }

    /**
     * 删除文件上传进度
     *
     * @param dictType dictType
     * @param uuid     uuid
     */
    @DeleteMapping(value = "/uploadProcess")
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    public ResponseEntity<String> deleteUploadProcess(@RequestParam("dictType") String dictType, @RequestParam(name = "uuid", required = false) String uuid) {
        artifactPromotionService.deleteUploadProcess(dictType, uuid);
        return ResponseEntity.ok("");
    }

    @PostMapping(value = "/query/slice/download/info")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<ArtifactSliceDownloadInfoRes> querySliceDownloadInfo(@RequestBody ArtifactSliceDownloadInfoReq model) {
        return ResponseEntity.ok(artifactPromotionService.querySliceDownloadInfo(model));
    }

    @PostMapping(value = "/batch/query/slice/download/info")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<List<ArtifactSliceDownloadInfoRes>> batchQuerySliceDownloadInfo(@RequestBody List<ArtifactSliceDownloadInfoReq> models) {
        return ResponseEntity.ok(artifactPromotionService.batchQuerySliceDownloadInfo(models));
    }

    @GetMapping(value = "/slice/upload/info")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public Result<ArtifactSliceUploadInfoRes> querySliceUploadInfo() {
        return Result.success(artifactPromotionService.querySliceUploadInfo());
    }

    @PostMapping(value = "/slice/upload")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public Result<Boolean> sliceUpload(@ModelAttribute @Validated ArtifactSliceUploadReq model, String metaDataMap) {
        try {
            return Result.success(artifactPromotionService.sliceUpload(model, metaDataMap));
        } catch (Exception e) {
            return Result.error(e);
        }
    }
    @PostMapping(value = "/header/slice/upload", consumes = {"application/octet-stream"})
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public Result<Boolean> sliceUploadByHeader(
            @RequestHeader("storageId") String storageId,
            @RequestHeader("repositoryId") String repositoryId,
            @RequestHeader("path") String path,
            @RequestHeader("mergeId") String mergeId,
            @RequestHeader("chunkIndex") Integer chunkIndex,
            @RequestHeader("chunkIndexMax") Integer chunkIndexMax,
            @RequestHeader("originFileMd5") String originFileMd5,
            HttpServletRequest request) {
        try {
            final MockMultipartFile mockMultipartFile = new MockMultipartFile(UUID.randomUUID().toString(), request.getInputStream());
            final ArtifactSliceUploadReq model = new ArtifactSliceUploadReq();
            model.setStorageId(storageId);
            model.setRepositoryId(repositoryId);
            model.setPath(path);
            model.setMergeId(mergeId);
            model.setChunkIndex(chunkIndex);
            model.setChunkIndexMax(chunkIndexMax);
            model.setOriginFileMd5(originFileMd5);
            model.setFile(mockMultipartFile);
            return Result.success(artifactPromotionService.sliceUpload(model));
        } catch (Exception e) {
            log.error("通过Header传参方式，文件切片上传失败", e);
            return Result.error(e);
        }
    }



    private Map<String, String> parseMetadata(String metadataHeader) {
        Map<String, String> metadata = new HashMap<>();

        if (metadataHeader != null && !metadataHeader.isEmpty()) {
            String[] entries = metadataHeader.split(",");
            for (String entry : entries) {
                String[] keyValue = entry.split(" ");
                if (keyValue.length == 2) {
                    String key = keyValue[0];
                    String value = new String(Base64.getDecoder().decode(keyValue[1]), StandardCharsets.UTF_8);
                    metadata.put(key, value);
                }
            }
        }

        return metadata;
    }

    @PostMapping(value = "/slice/upload-web",consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repositoryId", pathKey = "path")
    public ResponseEntity<?> sliceUploadWeb3(MultipartHttpServletRequest request,
                                             @RequestParam("storageId") String storageId,
                                             @RequestParam("repositoryId") String repositoryId,
                                             @RequestParam("fileId") String fileId,
                                             @RequestParam("currentChunk") int currentChunk,
                                             @RequestParam("path") String path,
                                             @RequestParam("fileName") String fileName,
                                             @RequestParam("totalChunks") int totalChunks,
                                             @RequestParam("fileMd5") String fileMd5,
                                             @RequestParam("currentChunkSize") long currentChunkSize,
                                             @RequestParam("chunkSize") long chunkSize,
                                             @RequestParam("chunkMD5") String chunkMD5,
                                             @RequestParam("file") MultipartFile file,
                                             @RequestParam("isUnzip") boolean isUnzip,
                                             @RequestParam(name = "fileMetaDataMap", required = false) String fileMetaDataMap,
                                             @RequestParam(name = "imageTag", required = false) String imageTag,
                                             @RequestParam(name = "fileType", required = false) String fileType,
                                             @RequestParam(name = "originalFilename", required = false) String originalFilename
    ) {

        try {
            synchronized (fileMd5){
                String baseUrl = getBaseUrl();
                String token = securityComponent.generateUserToken();
                if (currentChunkSize != chunkSize) {
                    // 记录异常日志或抛出异常
                    throw new IllegalArgumentException("Chunk size does not match!");
                }
                final MockMultipartFile mockMultipartFile = new MockMultipartFile(UUID.randomUUID().toString(), file.getInputStream());
                final ArtifactSliceUploadWebReq model = new ArtifactSliceUploadWebReq();
                model.setStorageId(storageId);
                model.setRepositoryId(repositoryId);
                model.setFileName(fileName);
                model.setBaseUrl(baseUrl);
                model.setToken(token);
                model.setFileType(fileType);
                model.setImageTag(imageTag);
                if(fileMetaDataMap!=null && !StrUtil.isBlankOrUndefined(fileMetaDataMap)){
                    model.setMetaData(JSONUtil.parseObj(fileMetaDataMap));
                }
                model.setPath(path);
                model.setMergeId(fileMd5);
                model.setOriginalFilename(originalFilename==null?fileName:originalFilename);
                model.setChunkIndex(currentChunk);
                model.setChunkIndexMax(totalChunks);
                model.setOriginFileMd5(fileMd5);
                model.setFile(mockMultipartFile);
                model.setUnzip(isUnzip);
                return ResponseEntity.ok(Result.success(artifactPromotionService.webSliceUpload(model)));
            }

        } catch (IOException e) {
            log.error(e.getMessage(), e);
            return ResponseEntity.status(500).body("Failed to upload chunk: " + e.getMessage());
        }
    }
}
