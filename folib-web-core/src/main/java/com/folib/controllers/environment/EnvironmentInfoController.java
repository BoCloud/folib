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
package com.folib.controllers.environment;

import com.folib.controllers.BaseController;
import com.folib.storage.Storage;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.YamlUserService.Yaml;

import javax.inject.Inject;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.*;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/environment/info")
@Api(description="环境参数控制器",tags = "环境参数控制器")
public class EnvironmentInfoController
        extends BaseController
{

    private static final String SYSTEM_PROPERTIES_PREFIX = "-D";

    @Inject
    @Yaml
    private UserService userService;

    @ApiOperation(value = "List all the environment variables, system properties and JVM arguments.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The list was returned."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getEnvironmentInfo()
    {
        logger.info("Listing of all environment variables, system properties and JVM arguments");

        Map<String, List<?>> propertiesMap = new LinkedHashMap<>();
        propertiesMap.put("environment", getEnvironmentVariables());
        propertiesMap.put("system", getSystemProperties());
        propertiesMap.put("jvm", getJvmArguments());
        propertiesMap.put("folib", getFolibInfo());

        try
        {
            return ResponseEntity.ok(objectMapper.writeValueAsString(propertiesMap));
        }
        catch (JsonProcessingException e)
        {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(String.format("{ 'error': '%s' }", e.getMessage()));
        }
    }

    private List<EnvironmentInfo> getFolibInfo()
    {
        List<EnvironmentInfo> folibInfo = new ArrayList();

        Map<String, Storage> storageMap = getConfiguration().getStorages();
        Long repositoriesCount = storageMap.values()
                                           .stream()
                                           .map(e -> e.getRepository(e.getId()))
                                           .count();

        folibInfo.add(new EnvironmentInfo("repositories", String.valueOf(repositoriesCount)));

        Long storagesCount = storageMap.values()
                                       .stream()
                                       .count();

        folibInfo.add(new EnvironmentInfo("storages", String.valueOf(storagesCount)));

        Long usersCount = userService.getUsers()
                                     .getUsers()
                                     .stream()
                                     .count();

        folibInfo.add(new EnvironmentInfo("users", String.valueOf(usersCount)));


        return folibInfo;
    }

    private List<EnvironmentInfo> getEnvironmentVariables()
    {
        Map<String, String> environmentMap = System.getenv();

        return environmentMap.entrySet().stream()
                             .sorted(Map.Entry.comparingByKey(String::compareToIgnoreCase))
                             .map(e -> new EnvironmentInfo(e.getKey(), e.getValue()))
                             .collect(Collectors.toList());
    }

    private List<EnvironmentInfo> getSystemProperties()
    {
        Properties systemProperties = System.getProperties();

        return systemProperties.entrySet().stream()
                               .sorted(Comparator.comparing(e -> ((String) e.getKey()).toLowerCase()))
                               .map(e -> new EnvironmentInfo((String) e.getKey(), (String) e.getValue()))
                               .collect(Collectors.toList());
    }

    private List<String> getSystemPropertiesAsString()
    {
        List<EnvironmentInfo> systemProperties = getSystemProperties();

        return systemProperties.stream()
                               .map(e -> SYSTEM_PROPERTIES_PREFIX + e.getName() + "=" + e.getValue())
                               .collect(Collectors.toList());
    }

    private List<String> getJvmArguments()
    {
        RuntimeMXBean runtimeMxBean = ManagementFactory.getRuntimeMXBean();
        List<String> arguments = runtimeMxBean.getInputArguments();
        List<String> systemProperties = getSystemPropertiesAsString();

        return arguments.stream()
                        .filter(argument -> !systemProperties.contains(argument))
                        .sorted(String::compareToIgnoreCase)
                        .collect(Collectors.toList());

    }


}
