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
package com.folib.controllers.forms;

import com.folib.forms.configuration.MavenRepositoryConfigurationForm;
import com.folib.forms.configuration.NugetRepositoryConfigurationForm;
import com.folib.forms.configuration.RawRepositoryConfigurationForm;
import com.folib.util.FieldSpy;
import com.folib.controllers.BaseController;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.NugetLayoutProvider;
import com.folib.providers.RawLayoutProvider;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryPolicyEnum;
import com.folib.storage.repository.RepositoryStatusEnum;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.security.AuthoritiesProvider;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import io.swagger.annotations.*;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/formData")
@Api(description = "表单数据控制器",tags = "表单数据控制器")
public class FormDataController
        extends BaseController
{

    private static final String SEARCH_PARAM_NAME = "term";

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private StorageProviderRegistry storageProviderRegistry;


    @ApiOperation(value = "Used to retrieve all assignable user roles and privileges")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Collection of all assignable user roles and privileges") })
    @PreAuthorize("hasAuthority('CREATE_USER') or hasAuthority('UPDATE_USER')")
    @GetMapping(value = "/userFields",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getUserFields()
    {
        return ResponseEntity.ok(new FormDataValuesCollection(
                ImmutableList.of(FormDataValues.fromCollection("assignableRoles",
                                                               authoritiesProvider.getAssignableRoles()))));
    }

    @ApiOperation(value = "Used to retrieve collection of storage form data")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Collection of storage form data") })
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE') or hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @GetMapping(value = "/storageFields", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getStorageFields()
    {
        return ResponseEntity.ok(new FormDataValuesCollection(
                ImmutableList.of(FormDataValues.fromDescribableEnum("policy", RepositoryPolicyEnum.class),
                                 FormDataValues.fromDescribableEnum("status", RepositoryStatusEnum.class),
                                 FormDataValues.fromDescribableEnum("type", RepositoryTypeEnum.class),
                                 FormDataValues.fromCollection("storageProvider", storageProviderRegistry.getProviders().keySet()),
                                 FormDataValues.fromCollection("layout", Arrays.asList(
                                         FormDataValues.fromCollection(Maven2LayoutProvider.ALIAS,
                                                                       FieldSpy.getAllFieldsInfo(
                                                                               MavenRepositoryConfigurationForm.class)),
                                         FormDataValues.fromCollection(NugetLayoutProvider.ALIAS,
                                                                       FieldSpy.getAllFieldsInfo(
                                                                               NugetRepositoryConfigurationForm.class)),
                                         FormDataValues.fromCollection(RawLayoutProvider.ALIAS,
                                                                       FieldSpy.getAllFieldsInfo(
                                                                               RawRepositoryConfigurationForm.class)))))));
    }

    @ApiOperation(value = "Used to retrieve storage names")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Storage names") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_STORAGE_CONFIGURATION')")
    @GetMapping(value = "/storageNames", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getStorageNames(@ApiParam(value = "Storage name filter")
                                          @RequestParam(value = SEARCH_PARAM_NAME, required = false) String filter)
    {
        Set<String> storageNames = configurationManagementService.getConfiguration().getStorages().keySet();
        if (StringUtils.isNotBlank(filter))
        {
            storageNames = storageNames.stream()
                                       .filter(name -> StringUtils.containsIgnoreCase(name, filter))
                                       .collect(Collectors.toSet());
        }
        storageNames = ImmutableSet.copyOf(Iterables.limit(storageNames, 10));

        return ResponseEntity.ok(new FormDataValuesCollection(
                ImmutableList.of(FormDataValues.fromCollection("storageNames", storageNames))));
    }

    @ApiOperation(value = "Returns a list of repository names")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "List of repository names") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/repositoryNames", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRepositoryNames(@ApiParam(value = "Search for repository names in a specific storageId")
                                             @RequestParam(value = "storageId", required = false)
                                                     String storageId,
                                             @ApiParam(value = "Search for repository names")
                                             @RequestParam(value = SEARCH_PARAM_NAME, required = false)
                                                     String filter,
                                             @ApiParam(value = "Return the repository names formatted as storageId:repositoryId")
                                             @RequestParam(value = "withStorageId", required = false)
                                                     boolean withStorageId,
                                             @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                             @RequestParam(value = "type", required = false)
                                                     String type,
                                             @ApiParam(value = "Filter repository names by repository layout")
                                             @RequestParam(value = "layout", required = false)
                                                     String layout
                                            )
    {
        Collection<? extends Repository> repositories = new HashSet<>();

        if (StringUtils.isNotBlank(storageId))
        {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (storage != null)
            {
                repositories = storage.getRepositories().values();
            }
        }
        else
        {
            repositories = configurationManagementService.getConfiguration().getRepositories();
        }

        Set<String> repositoryNames = Collections.emptySet();

        if (repositories.size() > 0)
        {
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByTerm = StringUtils.isNotBlank(filter);
            boolean filterByLayout = StringUtils.isNotBlank(layout);

            repositoryNames = repositories.stream()
                                          .distinct()
                                          .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                                          .filter(r -> !filterByType || r.isType(type))
                                          .filter(r -> !filterByTerm ||
                                                       StringUtils.containsIgnoreCase(r.getId(), filter))
                                          .map(r -> withStorageId ? r.getStorageIdAndRepositoryId() : r.getId())
                                          .collect(Collectors.toSet());

        }

        return ResponseEntity.ok(new FormDataValuesCollection(
                ImmutableList.of(FormDataValues.fromCollection("repositoryNames", repositoryNames))));
    }


    @ApiOperation(value = "Returns a list of repository ")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "List of repository ") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/repositoryList", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRepositoryList(@ApiParam(value = "Search for repository names in a specific storageId")
                                             @RequestParam(value = "storageId", required = false)
                                             String storageId,
                                             @ApiParam(value = "Search for repository names")
                                             @RequestParam(value = SEARCH_PARAM_NAME, required = false)
                                             String filter,
                                             @ApiParam(value = "Return the repository names formatted as storageId:repositoryId")
                                             @RequestParam(value = "withStorageId", required = false)
                                             boolean withStorageId,
                                             @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                             @RequestParam(value = "type", required = false)
                                             String type,
                                             @ApiParam(value = "Filter repository names by repository layout")
                                             @RequestParam(value = "layout", required = false)
                                             String layout
    )
    {
        Collection<? extends Repository> repositories = new HashSet<>();

        if (StringUtils.isNotBlank(storageId))
        {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (storage != null)
            {
                repositories = storage.getRepositories().values();
            }
        }
        else
        {
            repositories = configurationManagementService.getConfiguration().getRepositories();
        }

//        Set<String> repositoryNames = Collections.emptySet();

        if (repositories.size() > 0)
        {
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByTerm = StringUtils.isNotBlank(filter);
            boolean filterByLayout = StringUtils.isNotBlank(layout);



            repositories = repositories.stream()
                    .distinct()
                    .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                    .filter(r -> !filterByType || r.isType(type))
                    .filter(r -> !filterByTerm || StringUtils.containsIgnoreCase(r.getId(), filter))
                    .collect(Collectors.toCollection(LinkedHashSet::new));

        }

        return ResponseEntity.ok(repositories);
    }




    @ApiOperation(value = "Returns a list of repository names in group repositories")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "List of repository names") })
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/repositoryNamesInGroupRepositories", produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getRepositoryNamesInGroup(
            @ApiParam(value = "Search for repository names in a specific storageId")
            @RequestParam(value = "storageId", required = false)
                    String storageId,
            @ApiParam(value = "Search for repository names in a specific groupRepositoryId")
            @RequestParam(value = "groupRepositoryId", required = false)
                    String groupRepositoryId,
            @ApiParam(value = "Search for repository names")
            @RequestParam(value = SEARCH_PARAM_NAME, required = false)
                    String filter)
    {
        Collection<Repository> repositories = new HashSet<>();

        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(groupRepositoryId))
        {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (storage != null)
            {
                Repository repository = storage.getRepository(groupRepositoryId);
                if (repository.isGroupRepository())
                {
                    repositories.add(repository);
                }
            }
        }
        else if (StringUtils.isNotBlank(storageId))
        {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (storage != null)
            {
                repositories = (Collection<Repository>) storage.getRepositories().values();
            }
        }
        else
        {
            repositories = configurationManagementService.getConfiguration().getGroupRepositories();
        }

        List<String> repositoryNames = Lists.newArrayList();

        if (repositories.size() > 0)
        {
            boolean filterByTerm = StringUtils.isNotBlank(filter);
            for (Repository repository : repositories) {
                configurationManager.resolveGroupRepository(repository, repositoryNames);
            }
            repositoryNames = repositoryNames.stream()
                                          .filter(str -> !filterByTerm || StringUtils.containsIgnoreCase(str, filter))
                                          // we only need the repository name here
                                          //.map(str -> str.contains(":") ? str.split(":")[1] : str)
                                          .collect(Collectors.toList());

        }

        return ResponseEntity.ok(new FormDataValuesCollection(
                ImmutableList.of(FormDataValues.fromCollection("repositoryNames", repositoryNames))));
    }
}
