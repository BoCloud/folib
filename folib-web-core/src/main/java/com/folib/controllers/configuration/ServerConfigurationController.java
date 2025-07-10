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

import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.components.common.CommonComponent;
import com.folib.configuration.Configuration;
import com.folib.controllers.support.BaseUrlEntityBody;
import com.folib.controllers.support.InstanceNameEntityBody;
import com.folib.controllers.support.PortEntityBody;
import com.folib.forms.configuration.*;
import com.folib.job.cron.services.CronTaskConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.support.ConfigurationException;
import com.folib.validation.RequestBodyValidationException;
import io.swagger.annotations.*;
import jakarta.inject.Inject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Collection;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/configuration/folib")
@Api(description = "服务器设置", tags = "服务器设置")
public class ServerConfigurationController
        extends BaseConfigurationController {

    static final String SUCCESSFUL_SAVE_SERVER_SETTINGS = "服务器设置已成功更新.";

    static final String FAILED_SAVE_SERVER_SETTINGS = "无法保存服务器设置，因为提交的表单包含错误!";

    static final String FAILED_EMPTY_FORM = "提供了空表格";

    private final Validator validator;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    private AuthorizationConfigService authorizationConfigService;
    @Inject
    @Lazy
    private CronTaskConfigurationService cronTaskConfigurationService;

    public ServerConfigurationController(ConfigurationManagementService configurationManagementService,
                                         @Qualifier("localValidatorFactoryBean") Validator validator) {
        super(configurationManagementService);
        this.validator = validator;
    }

    @ApiOperation(value = "Updates the instance name.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "实例名称已更新."),
            @ApiResponse(code = 400, message = "无法更新实例的名称.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_INSTANCE_NAME', 'GLOBAL_CONFIGURATION_MANAGE')")
    @PutMapping(value = "/instanceName",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity setInstanceName(@ApiParam(value = "The instance's name", required = true)
                                          @RequestBody InstanceNameEntityBody instanceNameEntityBody,
                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        try {
            configurationManagementService.setInstanceName(instanceNameEntityBody.getInstanceName());

            logger.info("Set instance's name to [{}].", instanceNameEntityBody.getInstanceName());

            return ResponseEntity.ok(getResponseEntityBody("The instance's name was updated.", accept));
        } catch (ConfigurationException e) {
            String message = "Could not update the instance's name.";

            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(getResponseEntityBody(message, accept));
        }
    }

    @ApiOperation(value = "Returns the instance name of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "", response = String.class),
            @ApiResponse(code = 404, message = "No value for instanceName has been defined yet.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_VIEW_INSTANCE_NAME', 'GLOBAL_CONFIGURATION_MANAGE')")
    @GetMapping(value = "/instanceName",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getInstanceName(@RequestHeader(HttpHeaders.ACCEPT) String accept) {
        String instanceName = configurationManagementService.getConfiguration().getInstanceName();
        if (instanceName != null) {
            return ResponseEntity.ok(getInstanceNameEntityBody(instanceName,
                    accept));
        } else {
            String message = "No value for instanceName has been defined yet.";

            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(getResponseEntityBody(message, accept));
        }
    }

    @ApiOperation(value = "Updates the base URL of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The base URL was updated."),
            @ApiResponse(code = 400, message = "Could not update the base URL of the service.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_BASE_URL', 'GLOBAL_CONFIGURATION_MANAGE')")
    @PutMapping(value = "/baseUrl",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity setBaseUrl(@ApiParam(value = "The base URL", required = true)
                                     @RequestBody BaseUrlEntityBody baseUrlEntity,
                                     @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        try {
            String newBaseUrl = baseUrlEntity.getBaseUrl();
            configurationManagementService.setBaseUrl(newBaseUrl);

            logger.info("Set baseUrl to [{}].", newBaseUrl);

            return ResponseEntity.ok(getResponseEntityBody("The base URL was updated.", accept));
        } catch (ConfigurationException e) {
            String message = "Could not update the base URL of the service.";

            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(getResponseEntityBody(message, accept));
        }
    }

    @ApiOperation(value = "Returns the base URL of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "", response = String.class),
            @ApiResponse(code = 404, message = "No value for baseUrl has been defined yet.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_VIEW_BASE_URL', 'GLOBAL_CONFIGURATION_MANAGE', 'ARTIFACTS_VIEW')")
    @GetMapping(value = "/baseUrl",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getBaseUrl(@RequestHeader(HttpHeaders.ACCEPT) String accept) {
        String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
        if (baseUrl != null) {
            return ResponseEntity.ok(getBaseUrlEntityBody(baseUrl, accept));
        } else {
            String message = "No value for baseUrl has been defined yet.";
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(getResponseEntityBody(message, accept));
        }
    }

    @ApiOperation(value = "Sets the port of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The port was updated."),
            @ApiResponse(code = 400, message = "Could not update the Folib port.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_PORT', 'GLOBAL_CONFIGURATION_MANAGE')")
    @PutMapping(value = "/port/{port}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity setPort(@ApiParam(value = "The port of the service", required = true)
                                  @PathVariable int port,
                                  @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        try {
            configurationManagementService.setPort(port);

            logger.info("Set port to {}. This operation will require a server restart.", port);

            return ResponseEntity.ok(getResponseEntityBody("The port was updated.", accept));
        } catch (ConfigurationException e) {
            String message = "Could not update the Folib port.";
            logger.error(message, e);

            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(getResponseEntityBody(message, accept));
        }
    }

    @ApiOperation(value = "Sets the port of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The port was updated."),
            @ApiResponse(code = 400, message = "Could not update the Folib port.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_PORT', 'GLOBAL_CONFIGURATION_MANAGE')")
    @PutMapping(value = "/port",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity setPort(@ApiParam(value = "The port of the service", required = true)
                                  @RequestBody PortEntityBody portEntity,
                                  @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        return setPort(portEntity.getPort(), accept);
    }

    @ApiOperation(value = "Returns the port of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "", response = String.class)})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_VIEW_PORT', 'GLOBAL_CONFIGURATION_MANAGE')")
    @GetMapping(value = "/port",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getPort(@RequestHeader(HttpHeaders.ACCEPT) String accept) {
        return ResponseEntity.ok(
                getPortEntityBody(configurationManagementService.getConfiguration().getPort(), accept));
    }

    @ApiOperation(value = "Returns the allowAnonymous of the service.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "", response = String.class)})
    @GetMapping(value = "/allowAnonymous",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getAllowAnonymous() {
        return ResponseEntity.ok(configurationManagementService.getConfiguration().getAdvancedConfiguration().isAllowAnonymous());
    }

    @ApiOperation(value = "Set global server settings.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_SAVE_SERVER_SETTINGS),
            @ApiResponse(code = 400, message = FAILED_SAVE_SERVER_SETTINGS)})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_BASE_URL', 'CONFIGURATION_SET_PORT', 'GLOBAL_CONFIGURATION_MANAGE')")
    @PostMapping(value = "/serverSettings",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity setServerSettings(@RequestBody ServerSettingsForm serverSettingsForm,
                                            BindingResult bindingResult,
                                            @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws Exception {
        if (serverSettingsForm == null) {
            return getBadRequestResponseEntity(FAILED_EMPTY_FORM, acceptHeader);
        }
        validateServerSettingsForm(serverSettingsForm, bindingResult);
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_SAVE_SERVER_SETTINGS, bindingResult);

        }
        commonComponent.updateServerSettings(serverSettingsForm);
        return getSuccessfulResponseEntity(SUCCESSFUL_SAVE_SERVER_SETTINGS, acceptHeader);
    }

    @PreAuthorize("hasAnyAuthority('ADMIN')")
    @GetMapping(value = "/resolveS3Bucket")
    public ResponseEntity resolveS3Bucket() {
        commonComponent.resolveS3Bucket();
        return ResponseEntity.ok("success");
    }

    private void validateServerSettingsForm(ServerSettingsForm form,
                                            BindingResult bindingResult) {
        if (!isProxyConfigurationFormEmpty(form.getProxyConfigurationForm())) {
            ValidationUtils.invokeValidator(validator, form, bindingResult,
                    ProxyConfigurationForm.ProxyConfigurationFormChecks.class);
        }

        if (!isSmtpConfigurationFormEmpty(form.getSmtpConfigurationForm())) {
            ValidationUtils.invokeValidator(validator, form, bindingResult,
                    SmtpConfigurationForm.SmtpConfigurationFormChecks.class);
        }

        ValidationUtils.invokeValidator(validator, form, bindingResult);

    }

    private boolean isProxyConfigurationFormEmpty(ProxyConfigurationForm form) {
        return Stream.of(form.getHost(), form.getPort(), form.getType())
                .allMatch(this::isNullOrEmpty);
    }

    private boolean isSmtpConfigurationFormEmpty(SmtpConfigurationForm form) {
        return Stream.of(form.getHost(), form.getPort(), form.getConnection())
                .allMatch(this::isNullOrEmpty);
    }

    private boolean isNullOrEmpty(Object object) {
        if (object instanceof String) {
            String strObject = (String) object;
            return StringUtils.isBlank(strObject);
        }
        if (object instanceof Collection) {
            Collection collectionObject = (Collection) object;
            return CollectionUtils.isEmpty(collectionObject);
        }
        return Objects.isNull(object);
    }


    @ApiOperation(value = "get instanceName.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_SAVE_SERVER_SETTINGS),
            @ApiResponse(code = 400, message = FAILED_SAVE_SERVER_SETTINGS)})
    @GetMapping(value = "/getServerName",
            produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getServerName() {

        Configuration configuration = configurationManagementService.getConfiguration();
        return ResponseEntity.ok(configuration.getInstanceName());
    }

    @ApiOperation(value = "get version.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_SAVE_SERVER_SETTINGS),
            @ApiResponse(code = 400, message = FAILED_SAVE_SERVER_SETTINGS)})
    @GetMapping(value = "/getVersion",
            produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getVersion() {
        Configuration configuration = configurationManagementService.getConfiguration();
        return ResponseEntity.ok(configuration.getVersion());
    }

    @ApiOperation(value = "Get global server settings.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = SUCCESSFUL_SAVE_SERVER_SETTINGS),
            @ApiResponse(code = 400, message = FAILED_SAVE_SERVER_SETTINGS)})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_SET_BASE_URL', 'CONFIGURATION_SET_PORT', 'GLOBAL_CONFIGURATION_MANAGE')")
    @GetMapping(value = "/serverSettings",
            produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity getServerSettings(Model model) {

        Configuration configuration = configurationManagementService.getConfiguration();

        ServerSettingsForm settings = new ServerSettingsForm();
        settings.setBaseUrl(configuration.getBaseUrl());
        settings.setInstanceName(configuration.getInstanceName());
        settings.setPort(configuration.getPort());
        settings.setKbps(configuration.getKbps());
        settings.setSliceMbSize(configuration.getSliceMbSize());
        settings.setCorsConfigurationForm(
                CorsConfigurationForm.fromConfiguration(configuration.getCorsConfiguration())
        );
        settings.setSmtpConfigurationForm(
                SmtpConfigurationForm.fromConfiguration(configuration.getSmtpConfiguration())
        );
        settings.setProxyConfigurationForm(
                ProxyConfigurationForm.fromConfiguration(configuration.getProxyConfiguration())
        );
        settings.setAdvancedConfigurationForm(AdvancedConfigurationForm.fromConfiguration(configuration.getAdvancedConfiguration()));

        return ResponseEntity.ok(settings);
    }

    private Object getInstanceNameEntityBody(String instanceName,
                                             String accept) {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept)) {
            return new InstanceNameEntityBody(instanceName);
        } else {
            return instanceName;
        }
    }

    private Object getBaseUrlEntityBody(String baseUrl,
                                        String accept) {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept)) {
            return new BaseUrlEntityBody(baseUrl);
        } else {
            return baseUrl;
        }
    }

    private Object getPortEntityBody(int port,
                                     String accept) {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept)) {
            return new PortEntityBody(port);
        } else {
            return String.valueOf(port);
        }
    }
}
