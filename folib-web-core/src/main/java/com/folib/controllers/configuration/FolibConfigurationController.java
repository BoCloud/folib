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

import com.folib.configuration.MutableConfiguration;
import com.folib.controllers.ResponseMessage;
import com.folib.services.ConfigurationManagementService;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.IOException;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/configuration/folib")
@Api( tags = "修改系统配置", description= "修改系统配置")
public class FolibConfigurationController
        extends BaseConfigurationController
{

    public FolibConfigurationController(ConfigurationManagementService configurationManagementService)
    {
        super(configurationManagementService);
    }

    @ApiOperation(value = "上传 folib.yaml 并重新加载服务器的配置.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The configuration was updated successfully."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('CONFIGURATION_UPLOAD')")
    @PutMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE,
                             com.folib.net.MediaType.APPLICATION_YAML_VALUE },
            consumes = { MediaType.APPLICATION_JSON_VALUE,
                         com.folib.net.MediaType.APPLICATION_YAML_VALUE })
    public ResponseEntity<ResponseMessage> setFolibConfiguration(
            @ApiParam(value = "The folib.yaml configuration file", required = true) @RequestBody
                    MutableConfiguration configuration) throws IOException
    {
        configurationManagementService.setConfiguration(configuration);

        logger.info("通过 REST 接收新配置");

        return new ResponseEntity<>(ResponseMessage.empty().withMessage("配置更新成功."),
                                    HttpStatus.OK);
    }

    @ApiOperation(value = "检索 folib.yaml 配置文件。")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW')")
    @GetMapping(produces = { com.folib.net.MediaType.APPLICATION_YAML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<MutableConfiguration> getFolibConfiguration()
    {
        logger.info("Retrieved folib.yaml configuration file.");

        return new ResponseEntity<>(getMutableConfigurationClone(), HttpStatus.OK);
    }

}
