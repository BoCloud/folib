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

import com.folib.forms.routing.RoutingRuleForm;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.routing.MutableRoutingRule;
import com.folib.storage.routing.MutableRoutingRules;
import com.folib.validation.RequestBodyValidationException;

import java.io.IOException;
import java.util.UUID;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author Veadan
 * @author veadan
 */
@Controller
@RequestMapping("/api/configuration/folib/routing/rules")
@Api(description = "路由规则管理",tags = "路由规则管理")
public class RoutingConfigurationController
        extends BaseConfigurationController
{

    static final String SUCCESSFUL_ADD_ROUTING_RULE = "成功添加路由规则.";

    static final String FAILED_ADD_ROUTING_RULE_FORM_ERRORS = "提交的表单包含错误，无法添加路由规则!";

    static final String FAILED_ADD_ROUTING_RULE = "无法添加路由规则.";

    static final String SUCCESSFUL_REMOVE_ROUTING_RULE = "路由规则删除成功.";

    static final String FAILED_REMOVE_ROUTING_RULE = "无法删除路由规则.";

    static final String NOT_FOUND_REPOSITORY = "找不到路由规则.";

    static final String FAILED_UPDATE_ROUTING_RULE = "成功更新路由规则.";

    static final String FAILED_UPDATE_ROUTING_RULE_FORM_ERROR = "无法更新路由规则，因为提交的表单包含错误!";

    private final ConversionService conversionService;

    public RoutingConfigurationController(ConfigurationManagementService configurationManagementService,
                                          ConversionService conversionService)
    {
        super(configurationManagementService);
        this.conversionService = conversionService;
    }

    @ApiOperation(value = "返回 uuid 的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Everything went ok."),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY) })
    @GetMapping(value = "{uuid}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRoutingRule(@PathVariable UUID uuid,
                                         @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        MutableRoutingRule body = configurationManagementService.getRoutingRule(uuid);

        if (body == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_REPOSITORY, accept);
        }

        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "返回路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Everything went ok.") })
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRoutingRules()
    {
        MutableRoutingRules body = configurationManagementService.getRoutingRules();
        return ResponseEntity.ok(body);
    }

    @ApiOperation(value = "添加路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_ADD_ROUTING_RULE_FORM_ERRORS),
                            @ApiResponse(code = 404, message = FAILED_ADD_ROUTING_RULE) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity add(@RequestBody @Validated RoutingRuleForm routingRule,
                              BindingResult bindingResult,
                              @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_ADD_ROUTING_RULE_FORM_ERRORS, bindingResult);
        }

        MutableRoutingRule rule = conversionService.convert(routingRule, MutableRoutingRule.class);
        final boolean added = configurationManagementService.addRoutingRule(rule);

        return getResponse(added, SUCCESSFUL_ADD_ROUTING_RULE, FAILED_ADD_ROUTING_RULE, acceptHeader);
    }

    @ApiOperation(value = "删除提供了 uuid 的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REMOVE_ROUTING_RULE),
                            @ApiResponse(code = 404, message = FAILED_ADD_ROUTING_RULE) })
    @DeleteMapping(value = "/{uuid}",
                   produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity remove(@PathVariable UUID uuid,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        final boolean removed = configurationManagementService.removeRoutingRule(uuid);

        return getResponse(removed, SUCCESSFUL_REMOVE_ROUTING_RULE, FAILED_REMOVE_ROUTING_RULE, acceptHeader);
    }

    @ApiOperation(value = "更新指定索引处的路由规则.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = FAILED_UPDATE_ROUTING_RULE),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_ROUTING_RULE_FORM_ERROR),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY) })
    @PutMapping(value = "/{uuid}",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity update(@PathVariable UUID uuid,
                                 @RequestBody @Validated RoutingRuleForm routingRule,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_ROUTING_RULE_FORM_ERROR, bindingResult);
        }

        MutableRoutingRule rule = conversionService.convert(routingRule, MutableRoutingRule.class);

        final boolean updated = configurationManagementService.updateRoutingRule(uuid, rule);

        return getResponse(updated, FAILED_UPDATE_ROUTING_RULE, FAILED_UPDATE_ROUTING_RULE, acceptHeader);
    }

    private ResponseEntity getResponse(boolean result,
                                       String successfulMessage,
                                       String notFoundMessage,
                                       String acceptHeader)
    {
        if (result)
        {
            return getSuccessfulResponseEntity(successfulMessage, acceptHeader);
        }
        else
        {
            return getNotFoundResponseEntity(notFoundMessage, acceptHeader);
        }
    }

}
