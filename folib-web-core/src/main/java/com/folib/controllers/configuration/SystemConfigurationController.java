/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.controllers.configuration;

import com.folib.controllers.BaseController;
import com.folib.controllers.ResponseMessage;
import com.folib.model.request.ExportSystemConfigurationReq;
import com.folib.model.request.ImportSystemConfigurationReq;
import com.folib.services.SystemConfigurationService;
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
