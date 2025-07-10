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
package com.folib.controllers.configuration.security.authentication;

import javax.inject.Inject;

import com.folib.authentication.ConfigurableProviderManager;
import com.folib.authentication.api.AuthenticationItems;
import com.folib.controllers.BaseController;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * @author veadan
 * @author Veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/authenticators")
@Api(description = "用户权限管理",tags = "用户权限管理")
public class AuthenticatorsConfigController
        extends BaseController
{

    public static final String SUCCESSFUL_UPDATE = "更新成功";
    
    public static final String SUCCESSFUL_REORDER = "排序成功";

    public static final String FAILED_REORDER = "身份验证失败无法重新排序";
    
    public static final String FAILED_UPDATE= "无法更新用户配置";

    public static final String SUCCESSFUL_RELOAD = "重新加载认证配置成功";

    public static final String FAILED_RELOAD = "无法重新加载身份验证配置";

    @Inject
    private ConfigurableProviderManager providerManager;

    @ApiOperation(value = "枚举具有序号和名称的已排序身份验证器集合 ")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "成功返回列表") })
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public AuthenticationItems list()
    {
        return providerManager.getAuthenticationItems();
    }

    @ApiOperation(value = "按身份验证器的名称重新排序")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REORDER),
                            @ApiResponse(code = 400, message = FAILED_REORDER) })
    @PutMapping(path = "/reorder/{first}/{second}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity reorder(@PathVariable String first,
                                  @PathVariable String second,
                                  @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.reorder(first, second);
            return getSuccessfulResponseEntity(SUCCESSFUL_REORDER, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.BAD_REQUEST, FAILED_REORDER, e, acceptHeader);
        }
    }

    @ApiOperation(value = "根据身份验证器的索引重新排序")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_REORDER),
                            @ApiResponse(code = 400, message = FAILED_REORDER) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity update(@RequestBody AuthenticationItems authenticationItems,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.updateAuthenticationItems(authenticationItems);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.BAD_REQUEST, FAILED_UPDATE, e, acceptHeader);
        }
    }
    
    @ApiOperation(value = "重新加载的身份验证器注册")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_RELOAD),
                            @ApiResponse(code = 500, message = FAILED_RELOAD) })
    @PutMapping(path = "/reload",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity reload(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        try
        {
            providerManager.reload();
            return getSuccessfulResponseEntity(SUCCESSFUL_RELOAD, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_RELOAD, e, acceptHeader);
        }
    }
}
