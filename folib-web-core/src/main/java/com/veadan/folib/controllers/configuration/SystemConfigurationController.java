package com.veadan.folib.controllers.configuration;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.model.request.ExportSystemConfigurationReq;
import com.veadan.folib.model.request.ImportSystemConfigurationReq;
import com.veadan.folib.services.SystemConfigurationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/systemConfiguration")
@Api(description = "系统设置", tags = "系统设置")
public class SystemConfigurationController
        extends BaseController {

    @Autowired
    private SystemConfigurationService systemConfigurationService;

    @ApiOperation(value = "导出服务配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/export")
    public ResponseEntity<ResponseMessage> exportSystemConfiguration(@RequestBody @Validated ExportSystemConfigurationReq exportSystemConfigurationReq) {
        systemConfigurationService.exportSystemConfiguration(exportSystemConfigurationReq);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "导入服务配置")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/import")
    public ResponseEntity<ResponseMessage> importSystemConfiguration(@RequestBody @Validated ImportSystemConfigurationReq importSystemConfigurationReq) {
        systemConfigurationService.importSystemConfiguration(importSystemConfigurationReq);
        return ResponseEntity.ok(ResponseMessage.ok());
    }
}
