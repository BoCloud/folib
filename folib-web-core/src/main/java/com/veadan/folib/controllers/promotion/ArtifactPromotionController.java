package com.veadan.folib.controllers.promotion;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.model.request.ArtifactSupportSliceDownloadQueryReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceUploadInfoRes;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * 制品晋级控制层
 *
 * @author qijianping
 */
@RestController
@RequestMapping("/api/artifact/folib/promotion")
@Api(description = "制品文件管理", tags = "制品文件管理")
@Slf4j
public class ArtifactPromotionController extends BaseArtifactController {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

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


    @PostMapping("/nodeOption")
    @PermissionCheck(resourceKey = "ARTIFACTS_PROMOTION")
    public ResponseEntity nodeOption(@RequestBody @Validated PromotionNodeOption promotionNodeOption,
                                     HttpServletRequest request,
                                     BindingResult bindingResult) {
        logger.info("NodeOption params [{}]", JSONObject.toJSONString(promotionNodeOption));
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.nodeOptionAttachRecord(promotionNodeOption, request);
    }


///    @PostMapping("/nodeOptionCallback")
///    @PermissionCheck(resourceKey = "ARTIFACTS_PROMOTION")
///    public ResponseEntity<Boolean> nodeOptionCallback(@RequestBody @Validated ArtifactPromotionNodeOptionCallbackReq model) {
///        return ResponseEntity.ok(artifactPromotionService.nodeOptionCallback(model));
///    }

    @GetMapping("/info/{syncNo}")
    @PermissionCheck(resourceKey = "ARTIFACTS_PROMOTION")
    public ResponseEntity artifactPromotionInfo(@PathVariable("syncNo") String syncNo) {
        return artifactPromotionService.artifactPromotionInfo(syncNo);
    }


    @PostMapping(value = "/upload-files")
    @ApiOperation(value = "文件上传(支持批量)", notes = "文件上传(支持批量)")
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repostoryId")
    public ResponseEntity upload(@RequestParam("files") MultipartFile[] files,
                                 @RequestParam("storageId") String storageId,
                                 @RequestParam("repostoryId") String repositoryId,
                                 @RequestParam("filePathMap") String filePathMap,
                                 @RequestParam(name = "fileMetaDataMap", required = false) String fileMetaDataMap,
                                 @RequestParam(name = "uuid", required = false) String uuid) {
        return artifactPromotionService.upload(files, storageId, repositoryId, filePathMap, fileMetaDataMap, uuid);
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

    @PostMapping(value = "/artifactDispatch")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity artifactDispatch(@RequestBody @Validated ArtifactDispatch artifactDispatch, HttpServletRequest request, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.artifactDispatchAttachRecord(artifactDispatch, request);
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


    @GetMapping(value = "/file/speedLimitDownload/{storageId}/{repositoryId}/{artifactPath:.+}")
    public void speedLimitDownload(@RepositoryMapping Repository repository,
                                   @PathVariable String artifactPath, @RequestParam("nodeMark") String nodeMark,
                                   HttpServletResponse response) {
        artifactPromotionService.speedLimitDownload(repository, artifactPath, nodeMark, response);
    }

    @GetMapping(value = "/file/speedLimitSliceDownload/{storageId}/{repositoryId}/{artifactPath:.+}")
    public void speedLimitSliceDownload(@RepositoryMapping Repository repository,
                                        @PathVariable String artifactPath,
                                        @RequestParam("nodeMark") String nodeMark,
                                        @RequestParam("artifactMd5") String artifactMd5,
                                        @RequestParam("startDownloadIndex") Long startDownloadIndex,
                                        @RequestParam("readLength") Long readLength,
                                        HttpServletResponse response) {
        artifactPromotionService.speedLimitSliceDownload(repository, artifactPath, nodeMark, artifactMd5,
                startDownloadIndex, readLength, response);
    }

    @PostMapping(value = "/query/support/slice/download")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<Boolean> querySupportSliceDownload(@RequestBody @Validated ArtifactSupportSliceDownloadQueryReq model) {
        return ResponseEntity.ok(artifactPromotionService.querySupportSliceDownload(model));
    }

    @PostMapping(value = "/batch/query/support/slice/download")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<Map<String, Boolean>> batchQuerySupportSliceDownload(@RequestBody @Validated List<ArtifactSupportSliceDownloadQueryReq> models) {
        return ResponseEntity.ok(artifactPromotionService.batchQuerySupportSliceDownload(models));
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
    public ResponseEntity<ArtifactSliceUploadInfoRes> querySliceUploadInfo() {
        return ResponseEntity.ok(artifactPromotionService.querySliceUploadInfo());
    }

    @PostMapping(value = "/slice/upload")
    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<Boolean> sliceUpload(@ModelAttribute @Validated ArtifactSliceUploadReq model) {
        return ResponseEntity.ok(artifactPromotionService.sliceUpload(model));
    }

    @PostMapping(value = "/header/slice/upload", consumes = {"application/octet-stream"})
//    @PermissionCheck(resourceKey = "ARTIFACTS_RESOLVE")
    public ResponseEntity<Boolean> sliceUploadByHeader(
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
            return ResponseEntity.ok(artifactPromotionService.sliceUpload(model));
        } catch (IOException e) {
            log.error("通过Header传参方式，文件切片上传失败", e);
            return ResponseEntity.internalServerError().build();
        }
    }
}
