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

import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.controllers.support.NumberOfConnectionsEntityBody;
import com.folib.controllers.support.PoolStatsEntityBody;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.repository.RepositoryData;
import com.folib.storage.repository.Repository;
import com.folib.web.RepoMapping;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import java.io.IOException;

import org.apache.http.pool.PoolStats;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

/**
 * @author veadan
 * @author Veadan
 */
@Controller
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/proxy/connection-pool")
@Api(description = "代理连接控制器",tags ="代理连接控制器" )
public class HttpConnectionPoolConfigurationManagementController
        extends BaseConfigurationController
{
    private final ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    public HttpConnectionPoolConfigurationManagementController(ConfigurationManagementService configurationManagementService,
                                                               ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService)
    {
        super(configurationManagementService);
        this.proxyRepositoryConnectionPoolConfigurationService = proxyRepositoryConnectionPoolConfigurationService;
    }

    @ApiOperation(value = "更新代理存储库的池连接池数")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "代理存储库的池连接数已成功更新."),
                            @ApiResponse(code = 400, message = "代理存储库没有关联的远程存储库"),
                            @ApiResponse(code = 404, message = " (storage/repository) 不存在") })
    @PutMapping(value = "{storageId}/{repositoryId}/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setNumberOfConnectionsForProxyRepository(@RepoMapping Repository repository,
                                                                   @PathVariable(value = "numberOfConnections") int numberOfConnections,
                                                                   @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException
    { 
        final RepositoryData immutableRepository = (RepositoryData) repository;
        final String storageId = immutableRepository.getStorage().getId();
        final String repositoryId = immutableRepository.getId();

        if (immutableRepository.getRemoteRepository() == null)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody("代理存储库没有关联的远程存储库.", accept));
        }

        configurationManagementService.setProxyRepositoryMaxConnections(storageId, repositoryId, numberOfConnections);
        proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(
                immutableRepository.getRemoteRepository().getUrl(),
                numberOfConnections);

        String message = "存储库的池连接数已成功更新.";

        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "获取代理存储库池统计信息")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "检索到的代理存储库池统计信息"),
                            @ApiResponse(code = 400,
                                         message = "存储库没有远程存储库!"),
                            @ApiResponse(code = 404,
                                    message = "The (storage/repository) does not exist!") })
    @GetMapping(value = "{storageId}/{repositoryId}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getPoolStatsForProxyRepository(@RepoMapping Repository repository,
                                                         @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        final RepositoryData immutableRepository = (RepositoryData) repository;
        if (immutableRepository.getRemoteRepository() == null)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody("存储库没有远程存储库!", accept));
        }

        PoolStats poolStats = proxyRepositoryConnectionPoolConfigurationService
                                      .getPoolStats(immutableRepository.getRemoteRepository()
                                                                       .getUrl());

        return ResponseEntity.ok(getPoolStatsEntityBody(poolStats, accept));
    }

    @ApiOperation(value = "更新代理存储库的默认连接数")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "代理存储库的默认连接数已成功更新。"),
                            @ApiResponse(code = 400,
                                         message = "无法更新代理存储库的默认连接数") })
    @PutMapping(value = "default/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setDefaultNumberOfConnectionsForProxyRepository(@PathVariable(value = "numberOfConnections")
                                                                                  int numberOfConnections,
                                                                          @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        proxyRepositoryConnectionPoolConfigurationService.setDefaultMaxPerRepository(numberOfConnections);
        String message = "代理存储库的默认连接数已成功更新.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "获取代理存储库的默认连接数")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "已检索到默认连接数."),
                            @ApiResponse(code = 400,
                                         message = "无法获取代理存储库的默认连接数.") })
    @GetMapping(value = "default-number",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getDefaultNumberOfConnectionsForProxyRepository(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        int defaultNumber = proxyRepositoryConnectionPoolConfigurationService.getDefaultMaxPerRepository();
        return ResponseEntity.ok(getNumberOfConnectionsEntityBody(defaultNumber, accept));
    }

    @ApiOperation(value = "更新代理存储库的最大连接数")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "代理存储库的最大连接数已成功更新."),
                            @ApiResponse(code = 400,
                                         message = "无法更新代理存储库的最大连接数.") })
    @PutMapping(value = "max/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setMaxNumberOfConnectionsForProxyRepository(@PathVariable(value = "numberOfConnections")
                                                                              int numberOfConnections,
                                                                      @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        proxyRepositoryConnectionPoolConfigurationService.setMaxTotal(numberOfConnections);
        String message = "代理存储库的最大连接数已成功更新.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "获取代理存储库的最大连接数")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "已检索到代理存储库的最大连接数."),
                            @ApiResponse(code = 400,
                                         message = "无法获得代理存储库的最大连接数.") })
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getMaxNumberOfConnectionsForProxyRepository(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        int maxNumberOfConnections = proxyRepositoryConnectionPoolConfigurationService.getTotalStats()
                                                                                      .getMax();
        return ResponseEntity.ok(getNumberOfConnectionsEntityBody(maxNumberOfConnections, accept));
    }

    private Object getNumberOfConnectionsEntityBody(int numberOfConnections, String accept)
    {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept))
        {
            return new NumberOfConnectionsEntityBody(numberOfConnections);
        }
        else
        {
            return String.valueOf(numberOfConnections);
        }
    }

    private Object getPoolStatsEntityBody(PoolStats poolStats, String accept)
    {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept))
        {
            return new PoolStatsEntityBody(poolStats);
        }
        else
        {
            return String.valueOf(poolStats);
        }
    }

}
