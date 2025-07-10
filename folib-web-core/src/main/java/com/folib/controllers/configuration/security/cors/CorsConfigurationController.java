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
package com.folib.controllers.configuration.security.cors;

import com.folib.forms.configuration.CorsConfigurationForm;
import com.folib.controllers.BaseController;
import com.folib.services.ConfigurationManagementService;

import javax.inject.Inject;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

/**
 * @author veadan
 * @author Veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/cors")
@Api(description = "CORS跨域管理",tags = "CORS跨域管理")
public class CorsConfigurationController
        extends BaseController
{
    public static final String SUCCESSFUL_UPDATE = "CORS 允许的来源已更新";

    public static final String FAILED_UPDATE = "无法更新 CORS 允许的来源";

    private static final Logger logger = LoggerFactory.getLogger(CorsConfigurationController.class);

    @Inject
    private CorsConfigurationSource corsConfigurationSource;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @ApiOperation(value = "返回允许的来源")
    @ApiResponses(value = @ApiResponse(code = 200, message = "Allowed origins."))
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getAllowedOrigins()
    {
        List<String> allowedOrigins = ((UrlBasedCorsConfigurationSource) corsConfigurationSource).getCorsConfigurations()
                                                                                                 .values()
                                                                                                 .stream()
                                                                                                 .flatMap(
                                                                                                         c -> c.getAllowedOrigins() !=
                                                                                                              null ?
                                                                                                              c.getAllowedOrigins().stream() :
                                                                                                              Stream.empty())
                                                                                                 .collect(
                                                                                                         Collectors.toList());
        return getJSONListResponseEntityBody("origins", allowedOrigins);
    }

    @ApiOperation(value = "Sets CORS allowed origins",
                  notes = "In the request body, put an array of all allowed origins like this example: " +
                          "allowedOrigins: [\"http://example-a.com/\",\"http://example-b.com/foo/bar\"] " +
                          "You can always provide [\"*\"] to allow all origins.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = CorsConfigurationController.SUCCESSFUL_UPDATE),
                            @ApiResponse(code = 400, message = CorsConfigurationController.FAILED_UPDATE) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setAllowedOrigins(@RequestBody @Validated CorsConfigurationForm corsSettingsForm,
                                            BindingResult bindingResult,
                                            @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        Optional<CorsConfiguration> corsConfiguration = ((UrlBasedCorsConfigurationSource) corsConfigurationSource).getCorsConfigurations()
                                                                                                                   .values()
                                                                                                                   .stream()
                                                                                                                   .findFirst();

        if (corsConfiguration.isPresent())
        {
            if (setAllowedOrigins(corsConfiguration.get(), corsSettingsForm.getAllowedOrigins()))
            {
                return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE, acceptHeader);
            }
        }

        return getBadRequestResponseEntity(FAILED_UPDATE, acceptHeader);
    }

    private boolean setAllowedOrigins(CorsConfiguration corsConfiguration,
                                      List<String> allowedOrigins)
    {
        try
        {
            configurationManagementService.setCorsAllowedOrigins(allowedOrigins);
        }
        catch (Exception ex)
        {
            logger.error("Unable to set CORS allowed origins", ex);
            return false;
        }

        corsConfiguration.setAllowedOrigins(allowedOrigins);

        return true;
    }

}
