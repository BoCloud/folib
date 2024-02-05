package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.syncartifact.SyncArtifactProvider;
import com.veadan.folib.components.syncartifact.SyncArtifactProviderRegistry;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.MetadataConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactStatistics;
import com.veadan.folib.domain.thirdparty.ArtifactInfo;
import com.veadan.folib.domain.thirdparty.ArtifactQuery;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.task.EventTask;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/artifact/")
@Api(description = "制品管理", tags = "制品管理")
public class ArtifactController extends BaseController {

    @Inject
    private ArtifactWebService artifactWebService;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private EventTask eventTask;

    private static final String STORAGE_NOT_FOUND = "The storage was not found.";

    private static final String REPOSITORY_NOT_FOUND = "The repository was not found.";

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
        logger.info("批量新增制品元数据 [{}]", JSON.toJSONString(list));
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
                                                  @RequestParam(name = "metadata", required = false) Boolean metadata,
                                                  @RequestParam(name = "batch", required = false) Integer batch) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        artifactWebService.buildGraphIndex(userDetails.getUsername(), storageId, repositoryId, path, metadata, batch);
        return ResponseEntity.ok("ok");
    }

    @ApiOperation(value = "根据压缩包生成制品信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repositoryId")
    @PostMapping(value = "/store")
    public ResponseEntity<Object> store(@RequestParam(name = "storageId") String storageId,
                                        @RequestParam(name = "repositoryId") String repositoryId,
                                        @RequestParam(name = "path", required = false) String path,
                                        @RequestParam(name = "uuid", required = false) String uuid, @RequestParam(name = "file") MultipartFile file) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
        }
        if (!hasAdmin() && needValidatePathPrivileges(storageId, repositoryId)) {
            if (StringUtils.isBlank(path)) {
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("在此仓库中您的操作受限，请填写目标目录后再上传");
            } else {
                if (!validatePathPrivileges(storageId, repositoryId, Lists.newArrayList(path), Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                    return ResponseEntity.status(HttpStatus.FORBIDDEN).body("没有权限操作");
                }
            }
        }
        return ResponseEntity.ok(artifactWebService.store(userDetails.getUsername(), storageId, repositoryId, path, uuid, file));
    }

    @PostMapping(value = "/syncArtifactProvider")
    @PreAuthorize("hasAuthority('ADMIN')")
    @ResponseBody
    public ResponseEntity<String> syncArtifactProvider(@Valid @RequestBody SyncArtifactForm syncArtifactForm, @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManager.getConfiguration().getStorage(syncArtifactForm.getStorageId());
        if (Objects.isNull(storage)) {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
        Repository repository = configurationManager.getRepository(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
        if (Objects.isNull(repository)) {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, REPOSITORY_NOT_FOUND, accept);
        }
        SyncArtifactProvider syncArtifactProvider = syncArtifactProviderRegistry.getProvider(repository.getLayout());
        if ("layout".equals(syncArtifactForm.getType())) {
            syncArtifactProvider.fullSync(syncArtifactForm);
        } else {
            syncArtifactProvider.browseFullSync(syncArtifactForm);
        }
        return ResponseEntity.ok("success");
    }

    @GetMapping(value = "/artifactStatistics")
    @PreAuthorize("hasAuthority('ADMIN')")
    public ResponseEntity<ArtifactStatistics> artifactStatistics() {
        return ResponseEntity.ok(artifactWebService.artifactStatistics());
    }

    @ApiOperation(value = "查询制品分页列表", response = ArtifactInfo.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/thirdParty/page")
    public TableResultResponse<ArtifactInfo> thirdPartyPage(ArtifactQuery artifactQuery) {
        return artifactWebService.thirdPartyPage(artifactQuery);
    }

    @PostMapping(value = "/cleanupRepository/{storageId}/{repositoryId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    @ResponseBody
    public ResponseEntity<String> cleanupRepository(@PathVariable(name = "storageId") String storageId,
                                                    @PathVariable(name = "repositoryId") String repositoryId,
                                                    @RequestParam(name = "deleteFile", required = false) Boolean deleteFile,
                                                    @RequestParam(name = "limit", required = false) Integer limit, @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        if (Objects.isNull(storage)) {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (Objects.isNull(repository)) {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, REPOSITORY_NOT_FOUND, accept);
        }
        artifactWebService.cleanupRepository(storageId, repositoryId, deleteFile, limit);
        return ResponseEntity.ok("success");
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/preview/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<List> preview(@PathVariable String artifactPath,
                                        @PathVariable String storageId,
                                        @PathVariable String repositoryId) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        return ResponseEntity.ok(artifactWebService.preview(repositoryPath));
    }

    @ApiOperation(value = "制品扫描")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/scan/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<String> scan(@PathVariable String artifactPath,
                                       @PathVariable String storageId,
                                       @PathVariable String repositoryId) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        artifactWebService.scan(repositoryPath);
        return ResponseEntity.ok("");
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/dumpHead")
    public ResponseEntity<String> dumpHead(@RequestParam(value = "filePath", required = false) String filePath) {
        return ResponseEntity.ok(artifactWebService.dumpHead(filePath));
    }

    @ApiOperation(value = "上传制品的bom文件")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PermissionCheck(resourceKey = "ARTIFACTS_DEPLOY", storageKey = "storageId", repositoryKey = "repositoryId")
    @PostMapping(value = "/bom/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<String> bomUpload(@PathVariable(name = "storageId") String storageId,
                                            @PathVariable(name = "repositoryId") String repositoryId,
                                            @PathVariable(name = "artifactPath") String artifactPath,
                                            @RequestParam(name = "storageId") String reqStorageId,
                                            @RequestParam(name = "repositoryId") String reqRepositoryId,
                                            @RequestParam(name = "file") MultipartFile file) {
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
        }
        RepositoryPath repositoryPath = artifactComponent.getRepositoryPath(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, storageId, repositoryId, artifactPath));
        }
        artifactWebService.bomUpload(repositoryPath, file);
        return ResponseEntity.ok("success");
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/releaseLock/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<String> releaseLock(@PathVariable String artifactPath,
                                              @PathVariable String storageId,
                                              @PathVariable String repositoryId) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        repositoryPathLock.unLock(repositoryPath);
        return ResponseEntity.ok("");
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/lockInfo")
    public ResponseEntity<Integer> lockInfo() {
        return ResponseEntity.ok(repositoryPathLock.getLockInfo());
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/handleEvent")
    public ResponseEntity<String> handleEvent(@RequestParam(value = "filename", required = false) String filename) {
        eventTask.handle(filename);
        return ResponseEntity.ok("");
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/cleanSnapshot/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<String> cleanSnapshot(@PathVariable String artifactPath,
                                                @PathVariable String storageId,
                                                @PathVariable String repositoryId) {
        artifactWebService.cleanSnapshot(storageId, repositoryId, artifactPath);
        return ResponseEntity.ok("");
    }

    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping(value = "/dockerLayoutUpgrade/{storageId}/{repositoryId}")
    public ResponseEntity<String> dockerLayoutUpgrade(@PathVariable String storageId,
                                                      @PathVariable String repositoryId,
                                                      @RequestParam(required = false, name = "override") Boolean override) throws Exception {
        artifactWebService.dockerLayoutUpgrade(storageId, repositoryId, override);
        return ResponseEntity.ok("");
    }
}
