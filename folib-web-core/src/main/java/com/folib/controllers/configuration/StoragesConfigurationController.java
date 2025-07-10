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

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.annotation.JsonView;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.enums.NotifyScopesTypeEnum;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.enums.StorageProviderEnum;
import com.folib.forms.configuration.*;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.*;
import com.google.common.collect.Lists;
import com.folib.annotation.LicenseAnnotation;
import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.components.common.CommonComponent;
import com.folib.components.repository.RepositoryComponent;
import com.folib.configuration.ConfigurationUtils;
import com.folib.constant.GlobalConstants;
import com.folib.domain.RepositoryPermission;
import com.folib.domain.RepositoryUser;
import com.folib.domain.User;
import com.folib.dto.PermissionsDTO;
import com.folib.dto.UserDTO;
import com.folib.entity.Resource;
import com.folib.event.repository.RepositoryEventListenerRegistry;
import com.folib.forms.common.StorageTreeForm;
import com.folib.layout.providers.CargoLayoutProvider;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.RpmLayoutProvider;
import com.folib.providers.storage.FileSystemStorageProvider;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.services.StorageManagementService;
import com.folib.services.support.ConfigurationException;
import com.folib.storage.Storage;
import com.folib.storage.StorageData;
import com.folib.storage.Views;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.domain.Users;
import com.folib.users.service.FolibUserService;
import com.folib.users.service.ResourceService;
import com.folib.users.service.RoleResourceRefService;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.validation.RequestBodyValidationException;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.glassfish.jersey.client.ClientProperties;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.validation.groups.Default;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Veadan
 */
@RestController
@RequestMapping("/api/configuration/folib/storages")
@Api(description = "存储控件设置", tags = "存储控件设置")
public class StoragesConfigurationController
        extends BaseConfigurationController {
    static final String SUCCESSFUL_SAVE_STORAGE = "The storage was created successfully.";

    static final String FAILED_SAVE_STORAGE_FORM_ERROR = "The storage cannot be created because the submitted form contains errors!";

    static final String FAILED_SAVE_STORAGE_ERROR = "The storage was not created.";

    static final String SUCCESSFUL_UPDATE_STORAGE = "The storage was updated successfully.";

    static final String FAILED_UPDATE_STORAGE_FORM_ERROR = "The storage cannot be updated because the submitted form contains errors!";

    static final String FAILED_UPDATE_STORAGE_ERROR = "The storage was not updated.";

    static final String FAILED_SAVE_REPOSITORY = "The repository cannot be saved because the submitted form contains errors!";

    static final String FAILED_SAVE_REPOSITORY_PERMISSION = "The repository permission cannot be saved because the submitted form contains errors!";

    static final String FAILED_SAVE_REPOSITORY_PERMISSION_USER = "仓库可见范围改为存储空间内，用户【%s】不属于该存储空间，需要先从授权列表中移除";

    static final String SUCCESSFUL_REPOSITORY_SAVE = "The repository was updated successfully.";

    static final String FAILED_REPOSITORY_SAVE = "The repository was not saved.";

    static final String SUCCESSFUL_STORAGE_REMOVAL = "The storage was removed successfully.";

    static final String SUCCESSFUL_REPOSITORY_REMOVAL = "The repository was removed successfully.";

    static final String SUCCESSFUL_REPOSITORY_CLEANUP = "The repository was cleanup successfully.";

    private static final String FAILED_STORAGE_REMOVAL = "Failed to remove the storage !";

    private static final String STORAGE_NOT_FOUND = "The storage was not found.";

    private static final String FAILED_REPOSITORY_REMOVAL = "Failed to remove the repository !";

    private static final String FAILED_STORAGE_REMOVAL_EXISTS_REPOSITORY = "删除存储空间失败，请先删除其下的仓库 !";

    private static final String FAILED_REPOSITORY_REMOVAL_EXISTS_GROUP_REPOSITORY = "删除仓库失败，存在关联组合库，请先从组合库[%s]中移除 !";

    private static final String PARAMS_ERROR = "参数错误，请检查参数！";

    private final StorageManagementService storageManagementService;

    private final RepositoryManagementService repositoryManagementService;

    private final ConversionService conversionService;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Autowired
    @RelationalDatabaseUserService.RelationalDatabase
    @Lazy
    private UserService userService;

    @Autowired
    private AuthorizationConfigService authorizationConfigService;
    @Autowired
    private CommonComponent commonComponent;
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Inject
    private FolibUserService folibUserService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Autowired
    private ResourceService resourceService;
    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;
    @Autowired
    private RepositoryComponent repositoryComponent;
    @Lazy
    @Autowired
    private ObjectMapper objectMapper;

    public StoragesConfigurationController(ConfigurationManagementService configurationManagementService,
                                           StorageManagementService storageManagementService,
                                           RepositoryManagementService repositoryManagementService,
                                           ConversionService conversionService) {
        super(configurationManagementService);
        this.storageManagementService = storageManagementService;
        this.repositoryManagementService = repositoryManagementService;
        this.conversionService = conversionService;
    }

    @LicenseAnnotation
    @ApiOperation(value = "Adds a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was created successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity createStorage(@RequestBody
                                        @Validated({Default.class,
                                                StorageForm.NewStorage.class,
                                                ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                            StorageForm storageForm,
                                        BindingResult bindingResult,
                                        @RequestHeader(HttpHeaders.ACCEPT)
                                                String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_SAVE_STORAGE_FORM_ERROR, bindingResult);
        }
        if(storageManagementService.getStorageCount()>3){
            return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "存储空间数量3已满，请删除存储空间后再添加", accept);
        }
        try {
            StorageDto storage = conversionService.convert(storageForm, StorageDto.class);
            if (StringUtils.isBlank(storage.getAdmin())) {
                storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            }
            if (StringUtils.isBlank(storage.getStorageProvider())) {
                storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
            }
            storageManagementService.createStorage(storage);

            return getSuccessfulResponseEntity(SUCCESSFUL_SAVE_STORAGE, accept);
        } catch (ConfigurationException | IOException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_SAVE_STORAGE_ERROR, e, accept);
        }
    }

    @ApiOperation(value = "Updates a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was updated successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(value = "{storageId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity updateStorage(
            @ApiParam(value = "The storageId", required = true)
            @PathVariable String storageId,
            @RequestBody @Validated({Default.class,
                    StorageForm.ExistingStorage.class,
                    ProxyConfigurationForm.ProxyConfigurationFormChecks.class}) StorageForm storageFormToUpdate,
            BindingResult bindingResult,
            @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException(FAILED_UPDATE_STORAGE_FORM_ERROR, bindingResult);
        }

        if (!StringUtils.equals(storageId, storageFormToUpdate.getId())) {
            return getNotFoundResponseEntity(FAILED_UPDATE_STORAGE_ERROR, accept);
        }

        try {
            StorageDto storage = conversionService.convert(storageFormToUpdate, StorageDto.class);
            if (StringUtils.isBlank(storage.getAdmin())) {
                storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            }
            storageManagementService.updateStorage(storage);
            return getSuccessfulResponseEntity(SUCCESSFUL_UPDATE_STORAGE, accept);
        } catch (ConfigurationException | IOException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_UPDATE_STORAGE_ERROR, e, accept);
        }
    }


    @JsonView(Views.ShortStorage.class)
    @ApiOperation(value = "Retrieve the basic info about storages.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/queryStorages", produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public TableResultResponse<Storage> getStorages(Authentication authentication,
                                                    @RequestParam(name = "page") Integer page,
                                                    @RequestParam(name = "limit") Integer limit,
                                                    @RequestParam(name = "storageId", required = false) String storageId) {

        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        if (StringUtils.isNotEmpty(storageId)) {
            storages = storages.stream().filter(storage -> storage.getId().contains(storageId)).collect(Collectors.toList());
        }

        if (CollectionUtils.isEmpty(storages)) {
            return new TableResultResponse<>(0, new ArrayList<>());
        }

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(storages, null);
            List<Storage> pageStorages = collect.stream().skip((long) (page - 1) * limit).limit(limit).collect(Collectors.toList());
            return new TableResultResponse<>(pageStorages.size(), pageStorages);
        }

        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storages);
        String username = "";
        if (Objects.nonNull(authentication)) {
            final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
            username = loggedUser.getUsername();
        }
        List<Storage> storagesList = storages;
        if (!hasAdmin()) {
            String finalUsername = username;
            storagesList = storages.stream().filter(s ->
                    (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())))
            ).collect(Collectors.toList());
        }
        List<Storage> pageStorages = storagesList.stream().skip((long) (page - 1) * limit).limit(limit).collect(Collectors.toList());
        return new TableResultResponse<>(storagesList.size(), pageStorages);

    }

    @JsonView(Views.ShortStorage.class)
    @ApiOperation(value = "Retrieve the basic info about storages.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStorages(Authentication authentication) {
        final List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(storages, null);
            StoragesOutput storagesOutput = new StoragesOutput(collect);
            return ResponseEntity.ok(storagesOutput);
        }

        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storages);
        String username = "";
        if (Objects.nonNull(authentication)) {
            final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
            username = loggedUser.getUsername();
        }
        StoragesOutput storagesOutput = new StoragesOutput(storages);
        if (!hasAdmin()) {
            List<Storage> list = storagesOutput.getStorages();
            String finalUsername = username;
            List<Storage> collect = list.stream().filter(s ->
                    (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())))
            ).collect(Collectors.toList());
            storagesOutput.setStorages(collect);
        }
        return ResponseEntity.ok(storagesOutput);
    }

    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/queryStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public TableResultResponse<Repository> getStoragesAndRepositories(@ApiParam(value = "Search for repository names in a specific storageId")
                                                                      @RequestParam(value = "storageId", required = false)
                                                                              String storageId,
                                                                      @ApiParam(value = "Filter repository names by name")
                                                                      @RequestParam(value = "name", required = false)
                                                                              String name,
                                                                      @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                                                      @RequestParam(value = "type", required = false)
                                                                              String type,
                                                                      @ApiParam(value = "Filter exclude repository names by type (i.e. hosted, group, proxy)")
                                                                      @RequestParam(value = "excludeType", required = false)
                                                                              String excludeType,
                                                                      @ApiParam(value = "Search for exclude repository names")
                                                                      @RequestParam(value = "excludeRepositoryId", required = false)
                                                                              String excludeRepositoryId,
                                                                      @ApiParam(value = "Filter repository names by repository layout")
                                                                      @RequestParam(value = "layout", required = false)
                                                                              String layout,
                                                                      @ApiParam(value = "Filter repository names by repository policy")
                                                                      @RequestParam(value = "policy", required = false)
                                                                              String policy, Authentication authentication,
                                                                      @RequestParam(name = "page") Integer page,
                                                                      @RequestParam(name = "limit") Integer limit) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        List<Repository> repositoriesList = new ArrayList<>();
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            Map<String, List<String>> storageRepMap = new HashMap<>();
            //获取匿名角色关联的存储空间
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(storages, storageRepMap);
            //获取匿名角色关联的仓库
            repositoryComponent.getAnonymousUserRepositories(storageId, name, type, excludeType, excludeRepositoryId, layout, policy, collect, storageRepMap, repositoriesList, storageTreeForms);

            List<Repository> pageRepository = repositoriesList.stream().skip((long) (page - 1) * limit).limit(limit).collect(Collectors.toList());

            if (CollectionUtils.isEmpty(repositoriesList)) {
                return new TableResultResponse<>(0, new ArrayList<>());
            }
            return new TableResultResponse<>(repositoriesList.size(), pageRepository);
        }

        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        if (CollectionUtil.isNotEmpty(storages)) {
            //查询数据库中存储空间绑定的用户
            storageManagementService.getStorageUsers(storages);
            boolean filterByUser = !hasAdmin();
            boolean filterByStorageId = StringUtils.isNotBlank(storageId);
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByExcludeType = StringUtils.isNotBlank(excludeType);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            boolean filterByName = StringUtils.isNotBlank(name);
            String excludedStorageId = "", excludedRepositoryId = "";
            if (filterByExcludeRepositoryId) {
                excludedStorageId = ConfigurationUtils.getStorageId(storageId, excludeRepositoryId);
                excludedRepositoryId = ConfigurationUtils.getRepositoryId(excludeRepositoryId);
            }
            String excludedStorageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(excludedStorageId, excludedRepositoryId);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(loggedUser.getUsername())) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                    .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username) || storage.getRepositoryUsers().contains(username));
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || (!r.getStorageIdAndRepositoryId().equalsIgnoreCase(excludedStorageIdAndRepositoryId)))
                        .filter(r -> !filterByExcludeType || !r.getType().equalsIgnoreCase(excludeType))
                        .filter(r -> !filterByName || r.getId().toLowerCase().contains(name.toLowerCase()))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (flag) {
                    repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || hasRepositoryResolve(item))).collect(Collectors.toList());
                }
                repositoriesList.addAll(repositories);
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout())
                        .scope(repository.getScope()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        List<Repository> pageRepository = repositoriesList.stream().skip((long) (page - 1) * limit).limit(limit).collect(Collectors.toList());

        if (CollectionUtils.isEmpty(repositoriesList)) {
            return new TableResultResponse<>(0, new ArrayList<>());
        }
        return new TableResultResponse<>(repositoriesList.size(), pageRepository);
    }

    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/getStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getStoragesAndRepositories(@ApiParam(value = "Search for repository names in a specific storageId")
                                                     @RequestParam(value = "storageId", required = false)
                                                             String storageId,
                                                     @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
                                                     @RequestParam(value = "type", required = false)
                                                             String type,
                                                     @ApiParam(value = "Filter exclude repository names by type (i.e. hosted, group, proxy)")
                                                     @RequestParam(value = "excludeType", required = false)
                                                             String excludeType,
                                                     @ApiParam(value = "Search for exclude repository names")
                                                     @RequestParam(value = "excludeRepositoryId", required = false)
                                                             String excludeRepositoryId,
                                                     @ApiParam(value = "Filter repository names by repository layout")
                                                     @RequestParam(value = "layout", required = false)
                                                             String layout,
                                                     @ApiParam(value = "Filter repository names by repository policy")
                                                     @RequestParam(value = "policy", required = false)
                                                             String policy, Authentication authentication) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        List<Repository> repositorieList = new ArrayList<>();

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            Map<String, List<String>> storageRepMap = new HashMap<>();
            //获取匿名角色关联的存储空间
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(storages, storageRepMap);
            //获取匿名角色关联的仓库
            repositoryComponent.getAnonymousUserRepositories(storageId, "",type, excludeType, excludeRepositoryId, layout, policy, collect, storageRepMap, repositorieList, storageTreeForms);

            return ResponseEntity.ok(storageTreeForms);
        }

        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storages);
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        if (CollectionUtil.isNotEmpty(storages)) {
            boolean filterByUser = !hasAdmin();
            boolean filterByStorageId = StringUtils.isNotBlank(storageId);
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByExcludeType = StringUtils.isNotBlank(excludeType);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            String excludedStorageId = "", excludedRepositoryId = "";
            if (filterByExcludeRepositoryId) {
                excludedStorageId = ConfigurationUtils.getStorageId(storageId, excludeRepositoryId);
                excludedRepositoryId = ConfigurationUtils.getRepositoryId(excludeRepositoryId);
            }
            String excludedStorageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(excludedStorageId, excludedRepositoryId);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(loggedUser.getUsername())) ||
                            (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                    .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username)
                        || storage.getRepositoryUsers().contains(username));
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || (!r.getStorageIdAndRepositoryId().equalsIgnoreCase(excludedStorageIdAndRepositoryId)))
                        .filter(r -> !filterByExcludeType || !r.getType().equalsIgnoreCase(excludeType))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (flag) {
                    repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || hasRepositoryResolve(item))).collect(Collectors.toList());
                }
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout())
                        .scope(repository.getScope()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        return ResponseEntity.ok(storageTreeForms);
    }

    @ApiOperation(value = "Retrieve the basic info about storages and repositories.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/getPermissionStoragesAndRepositories", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getPermissionStoragesAndRepositories(
            @ApiParam(value = "Filter repository names by type (i.e. hosted, group, proxy)")
            @RequestParam(value = "type", required = false)
                    String type,
            @ApiParam(value = "Search for exclude repository names")
            @RequestParam(value = "excludeRepositoryId", required = false)
                    String excludeRepositoryId,
            @ApiParam(value = "Filter repository names by repository layout")
            @RequestParam(value = "layout", required = false)
                    String layout,
            @ApiParam(value = "Filter repository names by repository policy")
            @RequestParam(value = "policy", required = false)
                    String policy, Authentication authentication) {
        List<Storage> storages = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
        List<Repository> repositorieList = new ArrayList<>();

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            Map<String, List<String>> storageRepMap = new HashMap<>();
            //获取匿名角色关联的存储空间
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(storages, storageRepMap);
            //获取匿名角色关联的仓库
            repositoryComponent.getAnonymousUserRepositories(null, "", type, null, excludeRepositoryId, layout, policy, collect, storageRepMap, repositorieList, storageTreeForms);

            return ResponseEntity.ok(storageTreeForms);
        }
        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storages);
        final SpringSecurityUser loggedUser = (SpringSecurityUser) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        if (CollectionUtil.isNotEmpty(storages)) {
            boolean filterByUser = !hasAdmin() && loggedUser.getRoles().stream().noneMatch(role -> SystemRole.ARTIFACTS_MANAGER.name().equals(role.getName()));
            boolean filterByType = StringUtils.isNotBlank(type);
            boolean filterByLayout = StringUtils.isNotBlank(layout);
            boolean filterByExcludeRepositoryId = StringUtils.isNotBlank(excludeRepositoryId);
            boolean filterByPolicy = StringUtils.isNotBlank(policy);
            String excludedStorageId = "", excludedRepositoryId = "";
            if (filterByExcludeRepositoryId) {
                excludedStorageId = ConfigurationUtils.getStorageId("", excludeRepositoryId);
                excludedRepositoryId = ConfigurationUtils.getRepositoryId(excludeRepositoryId);
            }
            String excludedStorageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(excludedStorageId, excludedRepositoryId);
            storages = storages.stream()
                    .distinct()
                    .filter(s -> !filterByUser || username.equals(s.getAdmin()))
                    .collect(Collectors.toCollection(LinkedList::new));
            StorageTreeForm storageTreeForm;
            List<Repository> repositories;
            for (Storage storage : storages) {
                boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username)
                        || storage.getRepositoryUsers().contains(username));
                storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
                repositories = new LinkedList<Repository>(storage.getRepositories().values());
                repositories = repositories.stream().distinct()
                        .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                        .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                        .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                        .filter(r -> !filterByExcludeRepositoryId || (!r.getStorageIdAndRepositoryId().equalsIgnoreCase(excludedStorageIdAndRepositoryId)))
                        .collect(Collectors.toCollection(LinkedList::new));
                if (flag) {
                    repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || hasRepositoryResolve(item))).collect(Collectors.toList());
                }
                storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout()).build()).collect(Collectors.toList()));
                storageTreeForms.add(storageTreeForm);
            }
        }
        return ResponseEntity.ok(storageTreeForms);
    }


    @JsonView(Views.LongStorage.class)
    @ApiOperation(value = "Retrieve the configuration of a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "The storage ${storageId} was not found.")})
    @PreAuthorize("hasAnyAuthority('CONFIGURATION_VIEW_STORAGE_CONFIGURATION', 'ARTIFACTS_VIEW')")
    @GetMapping(value = "/{storageId}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity etStorageResponseEntity(@ApiParam(value = "The storageId", required = true)
                                                   @PathVariable final String storageId,
                                                   @ApiParam(value = "The filter")
                                                   @RequestParam(value = "filter", required = false) Boolean filter,
                                                   Authentication authentication) throws JsonProcessingException {
        StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
        if (Objects.isNull(storage)) {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, MediaType.APPLICATION_JSON_VALUE);
        }
        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            Map<String, List<String>> storageRepMap = new HashMap<>();
            //获取匿名角色关联的存储空间
            List<Storage> collect = repositoryComponent.getAnonymousUserStorages(Collections.singletonList(storage), storageRepMap);
            //获取匿名角色关联的仓库
            if (CollectionUtils.isNotEmpty(collect)) {
                Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
                if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
                    List<String> anonymousRepositories = storageRepMap.get(storage.getId());
                    repositoryMap = repositoryMap.values().stream().filter(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || (CollectionUtils.isNotEmpty(anonymousRepositories) && anonymousRepositories.contains(item.getId()))).collect(Collectors.toMap(Repository::getId, Function.identity()));
                    storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
                }
            }

            StorageData storageData = new StorageData(storage);
            return ResponseEntity.ok(storageData);
        }
        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(Collections.singletonList(storage));
        String username = loginUsername();
        boolean flag = Boolean.TRUE.equals(filter) && !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isEmpty(storage.getUsers()) || (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username)) || storage.getRepositoryUsers().contains(username));
        if (flag) {
            Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
            if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
                repositoryMap = repositoryMap.values().stream()
                        .filter(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || hasRepositoryResolve(item))
                        .map(item -> {
                            if (RepositoryTypeEnum.PROXY.getType().equals(item.getType())) {
                                RepositoryDto repositoryDto = (RepositoryDto) item;
                                repositoryDto.setHealthStatus(remoteRepositoryAlivenessCacheManager.isAlive(((RepositoryDto) item).getRemoteRepository()));
                            }
                            return item;
                        })
                        .collect(Collectors.toMap(Repository::getId, Function.identity()));
                storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
            }
        }
        Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
        if (Objects.nonNull(repositoryMap) && CollectionUtils.isNotEmpty(repositoryMap.values())) {
            repositoryMap = repositoryMap.values().stream()
                    .map(item -> {
                        if (RepositoryTypeEnum.PROXY.getType().equals(item.getType())) {
                            RepositoryDto repositoryDto = (RepositoryDto) item;
                            repositoryDto.setHealthStatus(remoteRepositoryAlivenessCacheManager.isAlive(((RepositoryDto) item).getRemoteRepository()));
                        }
                        return item;
                    })
                    .collect(Collectors.toMap(Repository::getId, Function.identity()));
            storage.setRepositories((Map<String, RepositoryDto>) repositoryMap);
        }
        StorageData storageData = new StorageData(storage);
        return ResponseEntity.ok(storageData);
    }

    @ApiOperation(value = "Deletes a storage.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The storage was removed successfully."),
            @ApiResponse(code = 404, message = "The storage ${storageId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove storage ${storageId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_STORAGE_CONFIGURATION')")
    @DeleteMapping(value = "/{storageId}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity removeStorage(@ApiParam(value = "The storageId", required = true)
                                        @PathVariable final String storageId,
                                        @ApiParam(value = "Whether to force delete and remove the storage from the file system")
                                        @RequestParam(name = "force", defaultValue = "false") final boolean force,
                                        @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            try {
                StorageDto storageDto = getMutableConfigurationClone().getStorage(storageId);
                if (MapUtils.isNotEmpty(storageDto.getRepositories())) {
                    return getFailedResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_STORAGE_REMOVAL_EXISTS_REPOSITORY, accept);
                }
                if (force) {
                    storageManagementService.removeStorage(storageId);
                    repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(storageId, "");
                }
                configurationManagementService.removeStorage(storageId);

                logger.info("Removed storage {}.", storageId);
                return getSuccessfulResponseEntity(SUCCESSFUL_STORAGE_REMOVAL, accept);
            } catch (ConfigurationException | IOException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_STORAGE_REMOVAL, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Get repository list..")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/repositories/{storageId}/{repositoryType}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity repositories(@ApiParam(value = "The storageId", required = true)
                                       @PathVariable String storageId,
                                       @ApiParam(value = "The repositoryType", required = true)
                                       @PathVariable
                                               String repositoryType) {
        List<Repository> repositories = configurationManagementService.getRepositoriesWithType(storageId, repositoryType);
        List<RepositoryForm> repositoryForms = Optional.ofNullable(repositories).orElse(Lists.newArrayList()).stream().map(item -> {
            RepositoryForm repository = new RepositoryForm();
            BeanUtils.copyProperties(item, repository);
            repository.setStorageId(item.getStorage().getId());
            if (FileSystemStorageProvider.ALIAS.equalsIgnoreCase(item.getStorageProvider())) {
                repository.setBasedir("/storages/" + repository.getStorageId() + "/" + repository.getId());
            }
            return repository;
        }).collect(Collectors.toList());
        return ResponseEntity.ok(repositoryForms);
    }


    @LicenseAnnotation
    @ApiOperation(value = "Adds or updates a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove the repository ${repositoryId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addOrUpdateRepository(@ApiParam(value = "The storageId", required = true)
                                                @PathVariable String storageId,
                                                @ApiParam(value = "The repositoryId", required = true)
                                                @PathVariable
                                                        String repositoryId,
                                                @ApiParam(value = "The repository object", required = true)
                                                @RequestBody
                                                @Validated({Default.class,
                                                        ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                    RepositoryForm repositoryForm,
                                                BindingResult bindingResult,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }
            RepositoryDto repository = conversionService.convert(repositoryForm, RepositoryDto.class);
            if (Objects.isNull(repository)) {
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository params is null", accept);
            }
            repository.setSyncEnabled(repositoryForm.isSyncEnabled());
            if (repositoryForm.getArtifactMaxSize() == 0) {
                repository.setArtifactMaxSize(107374182400L);
            }

            Repository existRepository = storage.getRepository(repositoryId);
            //TODO (存储空间+仓库) 唯一性判断
            boolean result = Objects.nonNull(existRepository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
            if (result) {
                //判断重复
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, "The repository id already exists", accept);
            }
            try {
                logger.info("Creating repository {}:{}...", storageId, repositoryId);
                groupRepositoryValid(storageId, repository);
                configurationManagementService.saveRepository(storageId, repository);
                RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
                try {
                    if (!Files.exists(repositoryPath)) {
                        repositoryManagementService.createRepository(storageId, repositoryId);
                    }
                } catch (Exception ex) {
                    logger.error("Failed to create the repository path {}!", repositoryId, ex);
                    try {
                        configurationManagementService.removeRepository(storageId, repositoryId);
                    } catch (Exception e) {
                        logger.error("Failed to remove the repository {}!", repositoryId, e);
                    }
                    throw new RuntimeException(ex.getMessage());
                }
                LayoutProvider layoutProvider = null;
                if (Objects.isNull(existRepository) ||
                        repositoryDto.getLayout().equals(CargoLayoutProvider.ALIAS) ||
                        repositoryDto.getLayout().equals(RpmLayoutProvider.ALIAS)) {

                    // 空值检查
                    if (repositoryDto.getLayout() == null) {
                        throw new IllegalArgumentException("Repository layout cannot be null");
                    }

                    try {
                        layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                    } catch (Exception e) {
                        // 处理异常
                        throw new RuntimeException("Failed to get layout provider", e);
                    }

                    // 初始化数据
                    if (layoutProvider != null) {
                        if (!RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) ||
                                repositoryDto.getLayout().equals(CargoLayoutProvider.ALIAS) ||
                                repositoryDto.getLayout().equals(RpmLayoutProvider.ALIAS)) {
                            layoutProvider.initData(storageId, repositoryId);
                        }
                    }
                }

                String resourceId = storageId + "_" + repositoryId;
                Resource resource = resourceService.queryById(resourceId);
                if (Objects.equals(null, resource)) {
                    resourceService.insert(Resource.builder()
                            .id(resourceId.toUpperCase())
                            .storageId(storageId)
                            .repositoryId(repositoryId)
                            .build());
                }

                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (Exception e) {
                logger.error("Failed to save the repository {}!", repositoryId, e);
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Verify if the repository is alive.")
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PostMapping(value = "/{storageId}/{repositoryId}/alive",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<RepositoryAliveForm> aliveRepository(@ApiParam(value = "The storageId", required = true)
                                                               @PathVariable String storageId,
                                                               @ApiParam(value = "The repositoryId", required = true)
                                                               @PathVariable
                                                                       String repositoryId,
                                                               @ApiParam(value = "The repository object", required = true)
                                                               @RequestBody
                                                               @Validated({Default.class,
                                                                       ProxyConfigurationForm.ProxyConfigurationFormChecks.class})
                                                                   RepositoryForm repositoryForm,
                                                               BindingResult bindingResult,
                                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(PARAMS_ERROR, bindingResult);
            }
            RepositoryDto repository = conversionService.convert(repositoryForm, RepositoryDto.class);
            boolean isAlive = false;
            Response response = null;
            int statusCode = 0;
            try {
                if (Objects.nonNull(repository) && Objects.nonNull(repository.getRemoteRepository())) {
                    String repositoryProxyConfigParam = "";
                    if (Objects.nonNull(repository.getProxyConfiguration())) {
                        repositoryProxyConfigParam = JSONObject.toJSONString(repository.getProxyConfiguration());
                    }
                    Client client = clientPool.getRestClient(repositoryProxyConfigParam);
                    //连接建立超时时间
                    client.property(ClientProperties.CONNECT_TIMEOUT, commonComponent.getConnectTimeout());
                    //读取内容超时时间
                    client.property(ClientProperties.READ_TIMEOUT, commonComponent.getReadTimeout());
                    WebTarget target = client.target(repository.getRemoteRepository().getUrl());
                    commonComponent.authentication(target, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
                    response = target.request().head();
                    statusCode = response.getStatus();
                    isAlive = HttpStatus.OK.value() == statusCode || HttpStatus.MOVED_PERMANENTLY.value() == statusCode ||
                            HttpStatus.FOUND.value() == statusCode;
                    logger.info("Verify if the storage [{}] repository [{}] remoteUrl [{}] is alive responseStatus [{}] response [{}]", storageId, repositoryId, repository.getRemoteRepository().getUrl(), statusCode, response.readEntity(String.class));
                }
                return ResponseEntity.ok(RepositoryAliveForm.builder().alive(isAlive).statusCode(statusCode).build());
            } catch (Exception e) {
                logger.info("Verify if the storage [{}] repository [{}] is alive error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
                return ResponseEntity.ok(RepositoryAliveForm.builder().alive(false).build());
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "add repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/whites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody
                                              @Validated({RepositoryForm.WhiteGroup.class})
                                                  RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("新增仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "remove repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/whites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                                 @PathVariable String storageId,
                                                 @ApiParam(value = "The repositoryId", required = true)
                                                 @PathVariable
                                                         String repositoryId,
                                                 @ApiParam(value = "The repository object", required = true)
                                                 @RequestBody
                                                 @Validated({RepositoryForm.WhiteGroup.class})
                                                     RepositoryForm repositoryForm,
                                                 BindingResult bindingResult,
                                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("删除仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "add repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/blacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity addRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody
                                              @Validated({RepositoryForm.BlackGroup.class})
                                                  RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("新增仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.addRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "remove repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/blacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                                 @PathVariable String storageId,
                                                 @ApiParam(value = "The repositoryId", required = true)
                                                 @PathVariable
                                                         String repositoryId,
                                                 @ApiParam(value = "The repository object", required = true)
                                                 @RequestBody
                                                 @Validated({RepositoryForm.BlackGroup.class})
                                                     RepositoryForm repositoryForm,
                                                 BindingResult bindingResult,
                                                 @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("删除仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.removeRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "set repository whites.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/setWhites",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity setRepositoryWhites(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityWhites(repositoryForm.getVulnerabilityWhites());
                logger.info("设置仓库级别白名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityWhites(storageId, repositoryId, repository.getVulnerabilityWhites());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "set repository blacks.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/setBlacks",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity setRepositoryBlacks(@ApiParam(value = "The storageId", required = true)
                                              @PathVariable String storageId,
                                              @ApiParam(value = "The repositoryId", required = true)
                                              @PathVariable
                                                      String repositoryId,
                                              @ApiParam(value = "The repository object", required = true)
                                              @RequestBody RepositoryForm repositoryForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }

            try {
                RepositoryDto repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                repository.setVulnerabilityBlacks(repositoryForm.getVulnerabilityBlacks());
                logger.info("设置仓库级别黑名单 {}:{}...", storageId, repositoryId);
                configurationManagementService.setRepositoryVulnerabilityBlacks(storageId, repositoryId, repository.getVulnerabilityBlacks());
                repository = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (IOException | ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "Returns the configuration of a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully.", response = RepositoryDto.class),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_VIEW_REPOSITORY')")
    @GetMapping(value = "/{storageId}/{repositoryId}",
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getRepositoryResponseEntity(@RepoMapping(allowOutOfServiceRepository = true) Repository repository) {
        if (repository.isGroupRepository()) {
            repository = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId()).getRepository(repository.getId());
            Set<String> vulnerabilityWhites = repository.getVulnerabilityWhites(), vulnerabilityBlacks = repository.getVulnerabilityBlacks();
            List<String> storageAndRepositoryIdList = Lists.newArrayList();
            configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
            for (String storageAndRepositoryId : storageAndRepositoryIdList) {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManagementService.getMutableConfigurationClone().getStorage(sId).getRepository(rId);
                vulnerabilityWhites.addAll(subRepository.getVulnerabilityWhites());
                vulnerabilityBlacks.addAll(subRepository.getVulnerabilityBlacks());
            }
        }
        return ResponseEntity.ok(repository);
    }


    @ApiOperation(value = "Deletes a repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was deleted successfully."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!"),
            @ApiResponse(code = 500, message = "Failed to remove the repository ${repositoryId}!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_REPOSITORY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}",
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity removeRepository(@RepoMapping(allowOutOfServiceRepository = true) Repository repository,
                                           @ApiParam(value = "Whether to force delete the repository from the file system")
                                           @RequestParam(name = "force", defaultValue = "false") final boolean force,
                                           @RequestParam(name = "cleanup", defaultValue = "false") final boolean cleanup,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            List<Repository> repositoryList = configurationManagementService.getConfiguration().getGroupRepositoriesContaining(storageId, repositoryId);
            if (CollectionUtils.isNotEmpty(repositoryList)) {
                return getFailedResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, String.format(FAILED_REPOSITORY_REMOVAL_EXISTS_GROUP_REPOSITORY, repositoryList.stream().map(item -> String.format("%s:%s", item.getStorage().getId(), item.getId())).collect(Collectors.joining(","))), accept);
            }
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                    .getRepository(repositoryId);
            if (cleanup && Files.exists(repositoryPath)) {
                logger.info("Cleanup repository {}:{}.", storageId, repositoryId);
                repositoryManagementService.cleanupRepository(storageId, repository.getId());
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_CLEANUP, accept);
            }
            if (force && Files.exists(repositoryPath)) {
                repositoryManagementService.removeRepository(storageId, repository.getId());
                repositoryEventListenerRegistry.dispatchRepoDelteToCronJobDeleteEvent(storageId, repositoryId);
            }
            configurationManagementService.removeRepository(storageId, repositoryId);
            logger.info("Removed repository {}:{}.", storageId, repositoryId);

            return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_REMOVAL, accept);
        } catch (IOException | ConfigurationException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_REMOVAL, e, accept);
        }
    }


    @ApiOperation(value = "set repository permissions.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PostMapping(value = "/{storageId}/{repositoryId}/permission",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity repositoryPermission(@ApiParam(value = "The storageId", required = true)
                                               @PathVariable String storageId,
                                               @ApiParam(value = "The repositoryId", required = true)
                                               @PathVariable
                                                       String repositoryId,
                                               @ApiParam(value = "The repository object", required = true)
                                               @RequestBody
                                               @Validated RepositoryPermissionForm repositoryPermissionForm,
                                               BindingResult bindingResult,
                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY_PERMISSION, bindingResult);
            }
            RepositoryPermissionDto repositoryPermissionDto = conversionService.convert(repositoryPermissionForm, RepositoryPermissionDto.class);
            if (Objects.isNull(repositoryPermissionDto)) {
                return getFailedResponseEntity(HttpStatus.BAD_REQUEST, FAILED_SAVE_REPOSITORY_PERMISSION, accept);
            }
      /*      if (RepositoryScopeEnum.STORAGE.getType().equals(repositoryPermissionDto.getScope())) {
                if (CollectionUtils.isNotEmpty(storage.getUsers())) {
                    //存储空间内，但是参数中包含了其他成员
                    List<RepositoryPermissionUserDto> userList = Optional.ofNullable(repositoryPermissionDto.getUserList()).orElse(Collections.emptyList()).stream().filter(item -> !storage.getUsers().contains(item.getUsername())).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(userList)) {
                        String uses = userList.stream().map(RepositoryPermissionUserDto::getUsername).collect(Collectors.joining(","));
                        return getFailedResponseEntity(HttpStatus.BAD_REQUEST, String.format(FAILED_SAVE_REPOSITORY_PERMISSION_USER, uses), accept);
                    }
                } else if (CollectionUtils.isNotEmpty(repositoryPermissionDto.getUserList())) {
                    String uses = repositoryPermissionDto.getUserList().stream().map(RepositoryPermissionUserDto::getUsername).collect(Collectors.joining(","));
                    return getFailedResponseEntity(HttpStatus.BAD_REQUEST, String.format(FAILED_SAVE_REPOSITORY_PERMISSION_USER, uses), accept);
                }
            }*/
            //TODO 检查用户的权限是否小于仓库权限
//            roleService.updateRepostoryPermission(storageId, repositoryId, repositoryPermissionDto);

            RepositoryDto repository = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId);
            repository.setScope(repositoryPermissionDto.getScope());
            repository.setAllowAnonymous(repositoryPermissionDto.isAllowAnonymous());
            groupRepositoryValid(storageId, repository);
            configurationManagementService.saveRepository(storageId, repository);
            repositoryManagementService.handlerRepositoryPermission(storageId, repositoryId, repositoryPermissionDto);
            return ResponseEntity.ok("ok");
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "get repository enable users.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/repositoryEnableUsers")
    public ResponseEntity repositoryEnableUsers(@ApiParam(value = "The storageId", required = true)
                                                @RequestParam String storageId,
                                                @ApiParam(value = "The repositoryId", required = true)
                                                @RequestParam String repositoryId, @ApiParam(value = "The scope") @RequestParam(name = "scope", required = false) Integer scope,
                                                @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            List<String> usernameList = null;
            Repository repository = storage.getRepository(repositoryId);
            if ((RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()) && !RepositoryScopeEnum.STORAGE.getType().equals(scope)) || RepositoryScopeEnum.OPEN.getType().equals(scope)) {
                //公开仓库，除了ADMIN、ARTIFACTS_MANAGER角色和存储空间管理员的其他所有人
                Users users = userService.getUsers();
                if (Objects.nonNull(users) && CollectionUtils.isNotEmpty(users.getUsers())) {
                    //排除有管理员、ARTIFACTS_MANAGER角色的用户
                    List<User> usersList = users.getUsers().stream().filter(item -> CollectionUtils.isNotEmpty(item.getRoles()) && item.getRoles().stream().noneMatch(role -> SystemRole.ADMIN.name().equals(role.getRoleName()) || SystemRole.ARTIFACTS_MANAGER.name().equals(role.getRoleName()))).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(usersList)) {
                        //排除存储空间管理员
                        usernameList = usersList.stream().filter(item -> !item.getUsername().equals(storage.getAdmin())).map(User::getUsername).collect(Collectors.toList());
                    }
                }
            } else {
                //存储空间下仓库，返回仓库成员
                if (CollectionUtils.isNotEmpty(storage.getUsers())) {
                    User user;
                    boolean flag;
                    usernameList = Lists.newArrayList();
                    for (String username : storage.getUsers()) {
                        if (username.equals(storage.getAdmin())) {
                            //过滤存储空间管理员
                            continue;
                        }
                        user = userService.findByUsername(username);
                        //过滤管理员、ARTIFACTS_MANAGER角色的用户
                        flag = Objects.nonNull(user) && CollectionUtils.isNotEmpty(user.getRoles()) && user.getRoles().stream().noneMatch(role -> SystemRole.ADMIN.name().equals(role.getRoleName()) || SystemRole.ARTIFACTS_MANAGER.name().equals(role.getRoleName()));
                        if (!flag) {
                            continue;
                        }
                        usernameList.add(user.getUsername());
                    }
                }
            }
            return ResponseEntity.ok(usernameList);
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "get repository enable users.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/repositoryUsers")
    public ResponseEntity repositoryUsers(@ApiParam(value = "The storageId", required = true) @RequestParam String storageId,
                                          @ApiParam(value = "The repositoryId", required = true) @RequestParam String repositoryId,
                                          @ApiParam(value = "The user") @RequestParam(name = "username", required = false) String username,
                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        List<String> usernameList = Lists.newArrayList();

        List<String> userNames = StrUtil.isNotEmpty(username) ? Lists.newArrayList(username) : Lists.newArrayList();
        List<UserDTO> userList = folibUserService.findByUserNameResource(userNames, null, null, null);
        Map<String, List<UserDTO>> manageUsers = userList.stream().filter(user -> user.getRoles().contains(SystemRole.ADMIN.name()) || user.getRoles().contains(SystemRole.REPOSITORY_MANAGER.name()) || user.getRoles().contains(String.format("STORAGE_ADMIN_%S", storageId))).collect(Collectors.groupingBy(UserDTO::getId));
        List<String> userIds = new ArrayList<>(manageUsers.keySet());
        userList.removeIf(user -> userIds.contains(user.getId()));
        if (CollectionUtils.isNotEmpty(userList)) {
            Map<String, List<UserDTO>> userMap = userList.stream().filter(user -> Objects.equals(user.getStorageId(), null) || storageId.equalsIgnoreCase(user.getStorageId()) && (repositoryId.equalsIgnoreCase(user.getRepositoryId()) || Objects.equals(user.getRepositoryId(), null))).collect(Collectors.groupingBy(UserDTO::getId));
            userMap.forEach((userId, users) -> {
                Set<String> storagePrivileges = users.stream().map(UserDTO::getStoragePrivilege).flatMap(Set::stream).collect(Collectors.toSet());
                if (!(storagePrivileges.contains(Privileges.ARTIFACTS_RESOLVE.name()) && storagePrivileges.contains(Privileges.ARTIFACTS_DELETE.name()) &&
                        storagePrivileges.contains(Privileges.ARTIFACTS_DEPLOY.name()) && storagePrivileges.contains(Privileges.ARTIFACTS_PROMOTION.name()))) {
                    usernameList.add(userId);
                }

            });
        }


        return ResponseEntity.ok(usernameList);

    }

    @ApiOperation(value = "get repository permission users.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/repositoryPermission")
    public ResponseEntity repositoryPermission(@ApiParam(value = "The storageId", required = true)
                                               @RequestParam String storageId,
                                               @ApiParam(value = "The repositoryId", required = true)
                                               @RequestParam
                                                       String repositoryId,
                                               @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (storage != null) {
            Repository repository = storage.getRepository(repositoryId);
            RepositoryPermission repositoryPermission = RepositoryPermission.builder().build();

            List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(null, null, storageId, null);
            //移除有管理角色的账户
            Map<String, List<PermissionsDTO>> managePermissionsMap = permissions.stream().filter(permission -> GlobalConstants.ROLE_TYPE_USER.equals(permission.getRefType()) &&
                    (SystemRole.ADMIN.name().equals(permission.getRoleId()) || SystemRole.REPOSITORY_MANAGER.name().equals(permission.getRoleId()) || String.format("STORAGE_ADMIN_%S", storageId).equals(permission.getRoleId()))
            ).collect(Collectors.groupingBy(PermissionsDTO::getEntityId));
            List<String> userIds = new ArrayList<>(managePermissionsMap.keySet());
            permissions.removeIf(permissionsDTO -> userIds.contains(permissionsDTO.getEntityId()));
            Map<String, List<PermissionsDTO>> permissionsMap = permissions.stream().filter(permission -> GlobalConstants.ROLE_TYPE_USER.equals(permission.getRefType())
            ).collect(Collectors.groupingBy(PermissionsDTO::getEntityId));
            List<RepositoryUser> userList = new ArrayList<>();
            permissionsMap.forEach((key, values) -> {
                List<PermissionsDTO> repositoryPermissions = values.stream().filter(permissionsDTO -> GlobalConstants.RESOURCE_TYPE_REPOSITORY.equals(permissionsDTO.getResourceType())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(repositoryPermissions)) {
                    List<String> repositoryPermissionList = repositoryPermissions.stream().flatMap(repositoryPer -> Stream.of(repositoryPer.getStoragePrivilege(), repositoryPer.getRepositoryPrivilege())).distinct().collect(Collectors.toList());
                    userList.add(RepositoryUser.builder().username(key).permissions(repositoryPermissionList).build());
                }
                Map<String, List<PermissionsDTO>> pathPermissions = values.stream().filter(permissionsDTO -> GlobalConstants.RESOURCE_TYPE_PATH.equals(permissionsDTO.getResourceType())).collect(Collectors.groupingBy(PermissionsDTO::getPath));
                pathPermissions.forEach((path, pathPermission) -> {
                    List<String> pathPermissionList = pathPermission.stream().map(PermissionsDTO::getPathPrivilege).distinct().collect(Collectors.toList());
                    userList.add(RepositoryUser.builder().username(key).permissions(pathPermissionList).paths(path).build());
                });
            });

            repositoryPermission.setScope(repository.getScope());
            repositoryPermission.setAllowAnonymous(repository.isAllowAnonymous());
            repositoryPermission.setUserList(userList);
            return ResponseEntity.ok(repositoryPermission);
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    @ApiOperation(value = "delete users repository permission.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "ok."),
            @ApiResponse(code = 404, message = "The repository ${storageId}:${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @DeleteMapping(value = "/repositoryPermission")
    public ResponseEntity deletePermission(@ApiParam(value = "The storageId", required = true)
                                           @RequestParam String storageId,
                                           @ApiParam(value = "The repositoryId", required = true)
                                           @RequestParam String repositoryId,
                                           @ApiParam(value = "The username", required = true)
                                           @RequestParam String username,
                                           @ApiParam(value = "The permissions", required = true)
                                           @RequestParam String permissions,
                                           @ApiParam(value = "The path")
                                           @RequestParam(required = false) String path,
                                           @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        List<String> privileges = Arrays.asList(permissions.split(","));
        List<PermissionsDTO> permissionList = roleResourceRefService.queryPermissions(null, username, storageId, repositoryId, false);
        List<PermissionsDTO> removeRefs;
        if (StringUtils.isNotEmpty(path)) {
            removeRefs = permissionList.stream().filter(per -> privileges.contains(per.getPathPrivilege())).map(permission -> {
                if (GlobalConstants.RESOURCE_TYPE_PATH.equals(permission.getResourceType())) {
                    return permission;
                }
                return null;
            }).collect(Collectors.toList());
        } else {
            removeRefs = permissionList.stream().filter(per -> privileges.contains(per.getRepositoryPrivilege())).map(permission -> {
                if (GlobalConstants.RESOURCE_TYPE_REPOSITORY.equals(permission.getResourceType())) {
                    return permission;
                }
                return null;
            }).collect(Collectors.toList());
        }

        List<Long> removeIds = removeRefs.stream().map(PermissionsDTO::getId).collect(Collectors.toList());
        roleResourceRefService.deleteByIds(removeIds);
        return ResponseEntity.ok("ok");
    }


    @ApiOperation(value = "set union repository.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The repository was updated successfully."),
            @ApiResponse(code = 404, message = "The repository ${repositoryId} was not found!")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_REPOSITORY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/unionRepository",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity unionRepository(@ApiParam(value = "The storageId", required = true)
                                          @PathVariable String storageId,
                                          @ApiParam(value = "The repositoryId", required = true)
                                          @PathVariable
                                                  String repositoryId,
                                          @ApiParam(value = "The repository object", required = true)
                                          @RequestBody
                                          @Validated
                                              UnionRepositoryConfigurationForm unionRepositoryConfigurationForm,
                                          BindingResult bindingResult,
                                          @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (configurationManagementService.getConfiguration().getStorage(storageId) != null) {
            if (bindingResult.hasErrors()) {
                throw new RequestBodyValidationException(FAILED_SAVE_REPOSITORY, bindingResult);
            }
            try {
                Repository repository = getConfiguration().getRepository(storageId, repositoryId);
                if (Objects.isNull(repository)) {
                    return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
                }
                logger.info("设置仓库的联邦仓库 {}:{}...", storageId, repositoryId);
                return getSuccessfulResponseEntity(SUCCESSFUL_REPOSITORY_SAVE, accept);
            } catch (ConfigurationException e) {
                return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_REPOSITORY_SAVE, e, accept);
            }
        } else {
            return getFailedResponseEntity(HttpStatus.NOT_FOUND, STORAGE_NOT_FOUND, accept);
        }
    }

    private void groupRepositoryValid(String storageId, Repository repository) {
        if (Objects.isNull(repository) || CollectionUtils.isEmpty(repository.getGroupRepositories())) {
            return;
        }
        String storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repository.getId());
        if (repository.getGroupRepositories().contains(storageIdAndRepositoryId)) {
            throw new IllegalArgumentException("The combination repository cannot contain itself");
        }
    }
}

