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

import com.folib.forms.configuration.ProxyConfigurationForm;
import com.folib.configuration.MutableProxyConfiguration;
import com.folib.services.ConfigurationManagementService;
import com.folib.validation.RequestBodyValidationException;

import javax.validation.groups.Default;

import io.swagger.annotations.*;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/configuration/folib/proxy-configuration")
@Api(description = "代理配置管理",tags="代理配置管理")
public class ProxyConfigurationController
        extends BaseConfigurationController
{

    static final String SUCCESSFUL_UPDATE = "代理配置已成功更新.";
    static final String FAILED_UPDATE_FORM_ERROR = "无法更新代理配置，因为提交的表单包含错误!";
    static final String FAILED_UPDATE = "更新代理配置失败!";

    static final String NOT_FOUND_PROXY_CFG = "找不到'${storageId}:${repositoryId}'的代理配置.";

    private final ConversionService conversionService;

    public ProxyConfigurationController(ConfigurationManagementService configurationManagementService,
                                        ConversionService conversionService)
    {
        super(configurationManagementService);
        this.conversionService = conversionService;
    }

    @ApiOperation(value = "更新存储库的代理配置（如果指定），否则更新全局代理设置.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_UPDATE),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_FORM_ERROR),
                            @ApiResponse(code = 500, message = FAILED_UPDATE) })
    @PreAuthorize("hasAuthority('CONFIGURATION_SET_GLOBAL_PROXY_CFG')")
    @PutMapping(value = "",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setProxyConfiguration(@ApiParam(value = "The storageId")
                                                @RequestParam(value = "storageId", required = false) String storageId,
                                                @ApiParam(value = "The repositoryId")
                                                @RequestParam(value = "repositoryId", required = false)
                                                        String repositoryId,
                                                @ApiParam(value = "此代理存储库的代理配置", required = true)
                                                @RequestBody @Validated({ Default.class,
                                                                          ProxyConfigurationForm.ProxyConfigurationFormChecks.class })
                                                    ProxyConfigurationForm proxyConfigurationForm,
                                                BindingResult bindingResult,
                                                @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {

        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_FORM_ERROR, bindingResult);
        }

        MutableProxyConfiguration proxyConfiguration = conversionService.convert(proxyConfigurationForm,
                                                                                 MutableProxyConfiguration.class);
        logger.info("收到代理配置\n: {}", proxyConfiguration);

        try
        {
            configurationManagementService.setProxyConfiguration(storageId, repositoryId, proxyConfiguration);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_UPDATE, e, acceptHeader);
        }
    }

    @ApiOperation(value = "如果已指定，则返回存储库的代理配置，否则返回全局代理设置.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 404, message = NOT_FOUND_PROXY_CFG) })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_GLOBAL_PROXY_CFG')")
    @GetMapping(value = "",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getProxyConfiguration(@ApiParam(value = "The storageId")
                                                @RequestParam(value = "storageId", required = false) String storageId,
                                                @ApiParam(value = "The repositoryId")
                                                @RequestParam(value = "repositoryId", required = false)
                                                        String repositoryId,
                                                @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        MutableProxyConfiguration proxyConfiguration;
        if (storageId == null)
        {
            proxyConfiguration = configurationManagementService.getMutableConfigurationClone()
                                                               .getProxyConfiguration();
        }
        else
        {
            proxyConfiguration = configurationManagementService.getMutableConfigurationClone()
                                                               .getStorage(storageId)
                                                               .getRepository(repositoryId)
                                                               .getProxyConfiguration();
        }

        if (proxyConfiguration != null)
        {
            return ResponseEntity.ok(proxyConfiguration);
        }
        else
        {
            String message = "代理配置" +
                             (storageId != null ? " for " + storageId + ":" + repositoryId : "") +
                             " was not found.";

            return getNotFoundResponseEntity(message, acceptHeader);
        }
    }
}
