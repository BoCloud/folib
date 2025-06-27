package com.veadan.folib.controllers.storagemonitoring;

import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.model.request.StorageMonitoringReq;
import com.veadan.folib.model.response.StorageMonitoringRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.StorageMonitoringService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.util.List;

/**
 * @author veadan
 */
@Slf4j
@RestController
@RequestMapping("/api/storageMonitoring/")
@Api(description = "存储监控", tags = "存储监控")
public class StorageMonitoringController extends BaseController {

    @Inject
    private StorageMonitoringService storageMonitoringService;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @ApiOperation(value = "查询存储监控列表", response = StorageMonitoring.class)
    @GetMapping(value = "/queryStorageMonitoring")
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    public ResponseEntity<List<StorageMonitoringRes>> queryStorageMonitoring(StorageMonitoring storageMonitoring) {
        return ResponseEntity.ok(storageMonitoringService.queryStorageMonitoring(storageMonitoring));
    }

    @ApiOperation(value = "查询存储监控分页列表", response = StorageMonitoring.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<StorageMonitoringRes> page(StorageMonitoringReq storageMonitoringReq) {
        return storageMonitoringService.queryStorageMonitoringPage(storageMonitoringReq);
    }

    @ApiOperation(value = "更新存储监控数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping(value = "/updateStorageMonitoringData")
    public ResponseEntity<String> updateStorageMonitoringData() {
        storageMonitoringService.updateStorageMonitoringData();
        return ResponseEntity.ok().build();
    }

}
