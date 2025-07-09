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
package com.folib.components.repository;

import cn.hutool.core.collection.CollectionUtil;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.components.auth.AuthComponent;
import com.folib.configuration.ConfigurationUtils;
import com.folib.dto.PermissionsDTO;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.forms.common.StorageTreeForm;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.StorageManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.service.RoleResourceRefService;
import com.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @date 2024/12/31
 **/
@Slf4j
@Component
public class RepositoryComponent {

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private RoleResourceRefService roleResourceRefService;

    @Inject
    @Lazy
    private StorageManagementService storageManagementService;

    @Inject
    @Lazy
    private AuthComponent authComponent;

    public List<Storage> getAnonymousUserStorages(List<Storage> storages, Map<String, List<String>> storageRepositoriesMap) {
        //匿名角色的权限
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(SystemRole.ANONYMOUS.name(), null, null, null, false);
        String one = "1";
        //有权限的存储空间集合
        List<String> storageIdList = Lists.newArrayList();
        //有权限的存储空间id:仓库id拼接集合
        List<String> storageAndRepositoryList = Lists.newArrayList();
        //有权限的存储空间和存储空间下仓库的数据
        Map<String, List<String>> storageRepMap = Maps.newLinkedHashMap();
        //存储空间、仓库、路径底下的权限
        List<PermissionsDTO> permissionsList = Optional.ofNullable(permissions).orElse(List.of()).stream().filter(item -> !one.equals(item.getResourceType())).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(permissionsList)) {
            boolean hasPermission;
            List<String> repositoryIdList;
            Map<String, List<String>> storageMap = storages.stream()
                    .collect(Collectors.toMap(
                            Storage::getId,
                            storage -> storage.getRepositories().values().stream()
                                    .map(Repository::getId)
                                    .collect(Collectors.toList())
                    ));
            for (PermissionsDTO permission : permissionsList) {
                hasPermission = Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getStoragePrivilege()) || Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getRepositoryPrivilege()) || Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getPathPrivilege());
                if (!hasPermission) {
                    continue;
                }
                if (!storageIdList.contains(permission.getStorageId())) {
                    storageIdList.add(permission.getStorageId());
                }
                if (Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getRepositoryPrivilege()) || Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getPathPrivilege())) {
                    if (StringUtils.isBlank(permission.getRepositoryId())) {
                        continue;
                    }
                    //拥有仓库下载权限
                    String storageAndRepository = ConfigurationUtils.getStorageIdAndRepositoryId(permission.getStorageId(), permission.getRepositoryId());
                    if (!storageAndRepositoryList.contains(storageAndRepository)) {
                        storageAndRepositoryList.add(storageAndRepository);
                    }
                    repositoryIdList = storageRepMap.get(permission.getStorageId());
                    if (CollectionUtils.isEmpty(repositoryIdList)) {
                        repositoryIdList = Lists.newArrayList();
                    }
                    if (!repositoryIdList.contains(permission.getRepositoryId())) {
                        repositoryIdList.add(permission.getRepositoryId());
                        storageRepMap.put(permission.getStorageId(), repositoryIdList);
                    }
                } else if (Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getStoragePrivilege())) {
                    //拥有存储空间下载权限
                    repositoryIdList = storageMap.get(permission.getStorageId());
                    if (CollectionUtils.isEmpty(repositoryIdList)) {
                        continue;
                    }
                    repositoryIdList.forEach(repo -> {
                        String storageAndRepository = ConfigurationUtils.getStorageIdAndRepositoryId(permission.getStorageId(), repo);
                        if (!storageAndRepositoryList.contains(storageAndRepository)) {
                            storageAndRepositoryList.add(storageAndRepository);
                        }
                    });
                    //存储空间下的所有仓库
                    storageRepMap.put(permission.getStorageId(), repositoryIdList);
                }
            }
            if (CollectionUtils.isNotEmpty(storageAndRepositoryList)) {
                for (Storage storage : storages) {
                    //查找符合条件的组合库
                    List<String> groupRepositoryIdList = storage.getRepositories().values().stream().filter(repo -> repo.isGroupRepository() && CollectionUtils.isNotEmpty(repo.getGroupRepositories()) && storageAndRepositoryList.stream().anyMatch(storageAndRepository -> repo.getGroupRepositories().contains(storageAndRepository))).map(Repository::getId).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(groupRepositoryIdList)) {
                        //原有仓库
                        repositoryIdList = storageRepMap.get(storage.getId());
                        if (CollectionUtils.isNotEmpty(repositoryIdList)) {
                            groupRepositoryIdList.addAll(repositoryIdList);
                            groupRepositoryIdList = groupRepositoryIdList.stream().distinct().collect(Collectors.toList());
                        }
                        storageRepMap.put(storage.getId(), groupRepositoryIdList);
                    }
                }
            }
        } else if (configurationManagementService.getConfiguration().getAdvancedConfiguration().isAllowAnonymous()) {
            //匿名角色没有配置存储空间、仓库、路径底下的权限。若全局允许匿名访问，获取允许匿名访问的仓库、公开仓库
            List<String> repositoryIdList;
            for (Storage storage : storages) {
                repositoryIdList = storage.getRepositories().values().stream().filter(repo -> repo.isAllowAnonymous() || RepositoryScopeEnum.OPEN.getType().equals(repo.getScope())).map(Repository::getId).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(repositoryIdList)) {
                    storageRepMap.put(storage.getId(), repositoryIdList);
                }
            }
        }
        if (storageRepositoriesMap != null) {
            storageRepositoriesMap.putAll(storageRepMap);
        }
        return storages.stream().filter(s -> (CollectionUtils.isNotEmpty(s.getRepositories().values())
                && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))) || storageIdList.contains(s.getId()) || storageRepMap.containsKey(s.getId())
        ).collect(Collectors.toList());
    }

    public void getAnonymousUserRepositories(String storageId, String name, String type, String excludeType, String excludeRepositoryId, String layout, String policy, List<Storage> collect, Map<String, List<String>> storageRepMap, List<Repository> repositoriesList, List<StorageTreeForm> storageTreeForms) {
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

        collect = collect.stream()
                .distinct()
                .filter(s -> !filterByStorageId || s.getId().equalsIgnoreCase(storageId))
                .collect(Collectors.toCollection(LinkedList::new));
        StorageTreeForm storageTreeForm;
        for (Storage storage : collect) {
            List<Repository> repositories;
            storageTreeForm = StorageTreeForm.builder().id(storage.getId()).key(storage.getId()).name(storage.getId()).build();
            repositories = new LinkedList<>(storage.getRepositories().values());
            repositories = repositories.stream().distinct()
                    .filter(Repository::isAllowAnonymous)
                    .filter(r -> !filterByType || r.getType().equalsIgnoreCase(type))
                    .filter(r -> !filterByLayout || r.getLayout().equalsIgnoreCase(layout))
                    .filter(r -> !filterByPolicy || r.getPolicy().equalsIgnoreCase(policy))
                    .filter(r -> !filterByExcludeRepositoryId || (!r.getStorageIdAndRepositoryId().equalsIgnoreCase(excludedStorageIdAndRepositoryId)))
                    .filter(r -> !filterByExcludeType || !r.getType().equalsIgnoreCase(excludeType))
                    .filter(r -> !filterByName || r.getId().toLowerCase().contains(name.toLowerCase()))
                    .collect(Collectors.toCollection(LinkedList::new));
            List<String> anonymousRepositories = storageRepMap.get(storage.getId());
            repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope())
                    || (CollectionUtils.isNotEmpty(anonymousRepositories) && anonymousRepositories.contains(item.getId()))))
                    .collect(Collectors.toList());
            repositoriesList.addAll(repositories);
            storageTreeForm.setChildren(repositories.stream().map(repository -> StorageTreeForm.builder().id(repository.getId()).key(storage.getId() + "," + repository.getId()).name(repository.getId()).type(repository.getType()).layout(repository.getLayout())
                    .scope(repository.getScope()).build()).collect(Collectors.toList()));
            storageTreeForms.add(storageTreeForm);
        }
    }

    public List<Repository> getUserRepositories(List<Storage> storageList) {
        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storageList);
        String username = UserUtils.getUsername();
        boolean isAdmin = !authComponent.hasAdmin();
        storageList = storageList.stream()
                .distinct()
                .filter(s -> !isAdmin || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(username)) ||
                        (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                .collect(Collectors.toCollection(LinkedList::new));
        List<Repository> repositories, repositoriesList = Lists.newArrayList();
        for (Storage storage : storageList) {
            boolean flag = !isAdmin && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username) || storage.getRepositoryUsers().contains(username));
            repositories = new LinkedList<Repository>(storage.getRepositories().values());
            repositories = repositories.stream().distinct()
                    .collect(Collectors.toCollection(LinkedList::new));
            if (flag) {
                repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()) || hasRepositoryResolve(item))).collect(Collectors.toList());
            }
            repositoriesList.addAll(repositories);
        }
        return repositoriesList;
    }

    public boolean hasRepositoryResolve(Repository repository) {
        return authComponent.validatePathPrivileges(repository, null, Privileges.ARTIFACTS_RESOLVE.name());
    }

    public List<Repository> getRepositories() {
        List<Repository> repositoriesList = Lists.newArrayList();
        List<Storage> storageList = Lists.newArrayList(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        String username = UserUtils.getUsername();
        if (StringUtils.isBlank(username)) {
            return repositoriesList;
        }
        String anonymousUser = "anonymousUser";
        if (anonymousUser.equals(username)) {
            Map<String, List<String>> storageRepMap = Maps.newHashMap();
            //获取匿名角色关联的存储空间
            List<Storage> anonymousUserStorageList = getAnonymousUserStorages(storageList, storageRepMap);
            List<StorageTreeForm> storageTreeForms = Lists.newArrayList();
            //获取匿名角色关联的仓库
            getAnonymousUserRepositories("", "","", "", "", "", "", anonymousUserStorageList, storageRepMap, repositoriesList, storageTreeForms);
        } else {
            repositoriesList = getUserRepositories(storageList);
        }
        return repositoriesList;
    }

    public List<String> getStorageIdAndRepositoryIdList() {
        List<Repository> repositoriesList = getRepositories();
        return Optional.ofNullable(repositoriesList).orElse(Lists.newArrayList()).stream().map(repository -> ConfigurationUtils.getSpecialStorageIdAndRepositoryId(repository.getStorage().getId(), repository.getId())).collect(Collectors.toList());
    }

}
