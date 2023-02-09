package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.configuration.MetadataConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/artifact")
@Api(value = "/api/artifact")
public class ArtifactController extends BaseController {

    @Inject
    private ArtifactWebService artifactWebService;

    @ApiOperation(value = "导出漏洞的影响范围")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/exportExcel")
    public void exportExcel(@RequestParam(name = "vulnerabilityUuid") String vulnerabilityUuid,
                            @RequestParam(name = "storageId", required = false) String storageId,
                            @RequestParam(name = "repositoryId", required = false) String repositoryId, HttpServletResponse response) throws IOException {
        artifactWebService.exportExcel(vulnerabilityUuid, storageId, repositoryId, response);
    }


    @ApiOperation(value = "全局设置添加或者更新元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_METADATA')")
    @PutMapping(value = "/globalSettingAddOrUpdateMetadata")
    public ResponseEntity<ResponseMessage> globalSettingAddOrUpdateMetadata(@RequestBody @Validated({ArtifactMetadataForm.ConfigurationAddOrUpdateGroup.class}) ArtifactMetadataForm artifactMetadataForm, BindingResult bindingResult) throws IOException {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        artifactWebService.globalSettingAddOrUpdateMetadata(artifactMetadataForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "全局设置删除元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_METADATA_CONFIGURATION')")
    @DeleteMapping(value = "/globalSettingDeleteMetadata")
    public ResponseEntity<ResponseMessage> globalSettingDeleteMetadata(@RequestBody @Validated({ArtifactMetadataForm.ConfigurationDeleteGroup.class}) ArtifactMetadataForm artifactMetadataForm, BindingResult bindingResult) throws IOException {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        artifactWebService.globalSettingDeleteMetadata(artifactMetadataForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "获取全局设置的元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK", response = MetadataConfiguration.class)})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_METADATA_CONFIGURATION')")
    @GetMapping(value = "/getMetadataConfiguration", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ArtifactMetadataForm>> getMetadataConfiguration() {
        return ResponseEntity.ok(artifactWebService.getMetadataConfiguration());
    }

    @ApiOperation(value = "新增制品元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_METADATA')")
    @PutMapping(value = "/artifactMetadata")
    public ResponseEntity<String> saveArtifactMetadata(@RequestBody @Validated({ArtifactMetadataForm.AddOrUpdateGroup.class}) ArtifactMetadataForm artifactMetadataForm, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        return ResponseEntity.ok(artifactWebService.saveArtifactMetadata(artifactMetadataForm));
    }

    @ApiOperation(value = "修改制品元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_METADATA')")
    @PostMapping(value = "/artifactMetadata")
    public ResponseEntity<String> updateArtifactMetadata(@RequestBody @Validated({ArtifactMetadataForm.AddOrUpdateGroup.class}) ArtifactMetadataForm artifactMetadataForm, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        return ResponseEntity.ok(artifactWebService.updateArtifactMetadata(artifactMetadataForm));
    }

    @ApiOperation(value = "删除制品元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_METADATA')")
    @PostMapping(value = "/deleteArtifactMetadata")
    public ResponseEntity<ResponseMessage> deleteArtifactMetadata(@RequestBody @Validated({ArtifactMetadataForm.DeleteGroup.class}) ArtifactMetadataForm artifactMetadataForm, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        artifactWebService.deleteArtifactMetadata(artifactMetadataForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "批量新增制品元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_METADATA')")
    @PostMapping(value = "/batchArtifactMetadata")
    public ResponseEntity<String> batchArtifactMetadata(@RequestBody @Validated({ArtifactMetadataForm.DeleteGroup.class}) List<ArtifactMetadataForm> list, BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(GlobalConstants.REQUEST_PARAMS_ERROR, bindingResult);
        }
        logger.info(" 批量新增制品元数据 {}", JSON.toJSONString(list));
        artifactWebService.batchArtifactMetadata(list);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "构建图数据库索引")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/buildGraphIndex")
    public ResponseEntity<String> buildGraphIndex(@RequestParam(name = "storageId", required = false) String storageId,
                                                  @RequestParam(name = "repositoryId", required = false) String repositoryId,
                                                  @RequestParam(name = "path", required = false) String path,
                                                  @RequestParam(name = "batch", required = false) Integer batch) throws Exception {
        artifactWebService.buildGraphIndex(storageId, repositoryId, path, batch);
        return ResponseEntity.ok("ok");
    }
}
