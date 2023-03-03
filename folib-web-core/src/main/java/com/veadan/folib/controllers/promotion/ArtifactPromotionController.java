package com.veadan.folib.controllers.promotion;

import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 制品晋级控制层
 *
 * @author qijianping
 */
@RestController
@RequestMapping("/api/artifact/folib/promotion")
@Api(value = "/api/artifact/folib/promotion")
@Slf4j
public class ArtifactPromotionController extends BaseArtifactController {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @PostMapping("/copy")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity copy(@RequestBody @Validated ArtifactPromotion artifactPromotion,
                               BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.copy(artifactPromotion);
    }

    @PostMapping("/move")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity move(@RequestBody @Validated ArtifactPromotion artifactPromotion, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.move(artifactPromotion);
    }

    // 节点晋级
    @PostMapping("/nodeOption")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity nodeOption(@RequestBody @Validated PromotionNodeOption promotionNodeOption,
                                     HttpServletRequest request,
                                     BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.nodeOption(promotionNodeOption, request);
    }

    // 上传接口
    @PostMapping(value = "/upload-files")
    @ApiOperation(value = "文件上传(支持批量)", notes = "文件上传(支持批量)")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity upload(@RequestParam("files") MultipartFile[] files,
                                 @RequestParam("storageId") String storageId,
                                 @RequestParam("repostoryId") String repositoryId,
                                 @RequestParam("filePathMap") String filePathMap,
                                 @RequestParam(name = "fileMetaDataMap", required = false) String fileMetaDataMap,
                                 @RequestParam(name = "uuid", required = false) String uuid) {
        return artifactPromotionService.upload(files, storageId, repositoryId, filePathMap, fileMetaDataMap, uuid);
    }

    // 下载接口
    @PostMapping(value = "/download")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity download(@RequestBody @Validated ArtifactDto artifactDto,
                                   HttpServletResponse response, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.download(artifactDto, response);
    }

    @PostMapping(value = "/getFileRelativePaths")
    @PermissionCheck(resourceKey = "CONFIGURATION_ADD_UPDATE_STORAGE")
    public ResponseEntity getFiles(@RequestBody @Validated ArtifactDto artifactDto,
                                   BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("请求参数错误", bindingResult);
        }
        return artifactPromotionService.getFileRelativePaths(artifactDto);
    }

    /**
     * 文件上传进度
     *
     * @param dictType dictType
     * @param uuid     uuid
     */
    @GetMapping(value = "/uploadProcess")
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
    public ResponseEntity<String> deleteUploadProcess(@RequestParam("dictType") String dictType, @RequestParam(name = "uuid", required = false) String uuid) {
        artifactPromotionService.deleteUploadProcess(dictType, uuid);
        return ResponseEntity.ok("");
    }
}
