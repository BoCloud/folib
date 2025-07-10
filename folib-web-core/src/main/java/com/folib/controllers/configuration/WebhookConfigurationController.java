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
import com.folib.entity.WebhookLog;
import com.folib.forms.configuration.WebhookConfigurationForm;
import com.folib.services.WebhookService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;

/**
 * @author leipenghui
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/folib/webhook")
@Api(description = "webhook管理",tags = "webhook管理")
public class WebhookConfigurationController extends BaseController {

    @Inject
    private WebhookService webhookService;

    @ApiOperation(value = "新增webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping
    public ResponseEntity<ResponseMessage> addWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.AddGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.addWebhookConfiguration(webhookConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "更新webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping
    public ResponseEntity<ResponseMessage> updateWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.UpdateGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.updateWebhookConfiguration(webhookConfigurationForm);
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "删除webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping
    public ResponseEntity<ResponseMessage> deleteWebhookConfiguration(@RequestBody @Validated(WebhookConfigurationForm.DeleteGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.deleteWebhookConfiguration(webhookConfigurationForm.getUuid());
        return ResponseEntity.ok(ResponseMessage.ok());
    }

    @ApiOperation(value = "查询webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping
    public ResponseEntity<List<WebhookConfigurationForm>> getWebhookConfiguration() throws IOException {
        return ResponseEntity.ok(webhookService.getWebhookConfiguration());
    }

    @ApiOperation(value = "查询webhookLog")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping("/log")
    public ResponseEntity<List<WebhookLog>> queryWebhookLog(WebhookLog webhookLog) throws IOException {
        return ResponseEntity.ok(webhookService.queryWebhookLogList(webhookLog));
    }

    @ApiOperation(value = "测试webhook")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping("/test")
    public ResponseEntity<Void> webhook(@RequestBody @Validated(WebhookConfigurationForm.TestGroup.class) WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        webhookService.testWebhook(webhookConfigurationForm);
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "删除webhookLog")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping("/log")
    public ResponseEntity<Void> deleteWebhookLog(@RequestBody WebhookLog webhookLog) throws IOException {
        webhookService.deleteWebhookLog(webhookLog);
        return ResponseEntity.ok().build();
    }
}
