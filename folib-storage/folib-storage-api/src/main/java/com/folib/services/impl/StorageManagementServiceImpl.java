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
package com.folib.services.impl;

import com.folib.domain.*;
import com.folib.entity.FolibRole;
import com.folib.entity.Resource;
import com.folib.entity.RoleResourceRef;
import com.folib.entity.UserGroupRef;
import com.folib.users.service.*;
import com.google.common.collect.Lists;
import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.RoleDto;
import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.constant.GlobalConstants;
import com.folib.converters.RoleModelToRoleConverter;
import com.folib.dto.PermissionsDTO;
import com.folib.enums.StorageProviderEnum;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.services.StorageManagementService;
import com.folib.storage.Storage;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.Repository;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.util.UserUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Component("storageManagementService")
public class StorageManagementServiceImpl implements StorageManagementService {

    protected static final Logger logger = LoggerFactory.getLogger(StorageManagementServiceImpl.class);

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    @Lazy
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    @Lazy
    private UserService userService;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private ResourceService resourceService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    private UserGroupRefService userGroupRefService;

    @Override
    public void updateStorage(StorageDto storage)
            throws IOException {
        handlerOriginalStorageAdminRoleByDB(storage.getAdmin(), storage.getId());
        configurationManagementService.updateStorage(storage);
        handlerStorageAdminRoleByDB(storage.getAdmin(), storage.getId());

    }

    @Override
    public void createStorage(StorageDto storage)
            throws IOException {
        configurationManagementService.createStorage(storage);
        handlerStorageAdminRoleByDB(storage.getAdmin(), storage.getId());
        handlerStorageOrdinaryRoleByDB(null, storage.getId());
    }

    @Override
    public void removeStorage(String storageId)
            throws IOException {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        for (Repository repository : storage.getRepositories().values()) {
            repositoryManagementService.removeRepository(storageId, repository.getId());
        }
        deleteStorageRole(storageId);
    }

    /**
     * 删除存储空间关联的角色和权限
     * @param storageId
     */
    private void deleteStorageRole(String storageId) {
        String adminRoleId = String.format("STORAGE_ADMIN_%S", storageId);
        String ordinalRoleId = String.format("STORAGE_USER_%S", storageId);
        //删除存储空间关联的角色
        folibRoleService.deleteById(adminRoleId);
        folibRoleService.deleteById(ordinalRoleId);
        //删除存储空间关联的权限
        List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefs(RoleResourceRef.builder().roleId(adminRoleId).build());
        List<RoleResourceRef> ordinalRoleResourceRefs = roleResourceRefService.queryRefs(RoleResourceRef.builder().roleId(ordinalRoleId).build());
        roleResourceRefs.addAll(ordinalRoleResourceRefs);
        if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
            roleResourceRefService.removeByIds(roleResourceRefs.stream().map(RoleResourceRef::getId).collect(Collectors.toList()));
        }
    }

    @Override
    public void handleStorageProvider(StorageDto storage) throws IOException {
        if (StringUtils.isNotBlank(storage.getBasedir())) {
            storage.setStorageProvider(StorageProviderEnum.S3.getType());
        } else {
            storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
        }
        logger.info("Storage [{}] storageProvider [{}]", storage.getId(), storage.getStorageProvider());
        updateStorage(storage);
    }

    @Override
    public void syncYamlStorageUsers(Collection<Storage> storages) {
        try {
            if (CollectionUtils.isNotEmpty(storages)) {
                List<Resource> resources = resourceService.findAll();
                List<String> storagetIds = resources.stream().map(Resource::getStorageId).filter(StringUtils::isNotEmpty).collect(Collectors.toList());
                List<String> repositoryIds = resources.stream().map(Resource::getRepositoryId).filter(StringUtils::isNotEmpty).collect(Collectors.toList());
                storages.forEach(storage -> {
                    List<Resource> resourceList = new ArrayList<>();

                    //添加存储空间、仓库资源
                    Set<String> repositories = storage.getRepositories().keySet();
                    String storageId = storage.getId();
                    resourceList.add(Resource.builder().storageId(storageId).build());
                    repositories.forEach(repositoryId -> resourceList.add(Resource.builder().storageId(storageId).repositoryId(repositoryId).build()));
                    List<Resource> addResources = resourceList.stream().filter(res -> {
                        String repositoryId = res.getRepositoryId();
                        String storageId1 = res.getStorageId();
                        return (repositoryId != null && !(repositoryIds.contains(repositoryId) && storagetIds.contains(storageId1)))
                                || (repositoryId == null && !storagetIds.contains(storageId1));
                    }).collect(Collectors.toList());
                    //添加资源
                    if (!addResources.isEmpty()) {
                        resourceService.saveBatch(addResources);
                    }

                    //添加存储空间管理员角色
                    String admin = storage.getAdmin();
                    handlerStorageAdminRoleByDB(admin, storage.getId());
                    Set<String> users = storage.getUsers();
    //                handlerStorageOrdinaryRoleByDB(users, storage.getId());

                    //创建角色
                    String roleId = String.format("STORAGE_USER_%S", storageId);
                    FolibRole folibRole = folibRoleService.queryById(roleId);
                    if (Objects.isNull(folibRole)) {
                        folibRole = FolibRole.builder()
                                .id(roleId)
                                .cnName(String.format("存储空间%s用户", storageId))
                                .enName(roleId)
                                .isDefault(GlobalConstants.NOT_DEFAULT).deleted(GlobalConstants.NOT_DELETED)
                                .description(String.format("存储空间%s下的普通用户", storageId))
                                .build();
                        folibRole = folibRoleService.insert(folibRole);
                    }
                    //添加用户和资源绑定
                    if (CollectionUtils.isNotEmpty(users)) {
                        List<RoleResourceRef> userRef = new ArrayList<>(users.size());
                        FolibRole finalFolibRole = folibRole;
                        users.forEach(user -> {
                            addResources.forEach(res -> {
                                String repositoryId = res.getRepositoryId();
                                if (StringUtils.isNotEmpty(repositoryId)) {
                                    userRef.add(RoleResourceRef.builder().roleId(finalFolibRole.getId()).resourceId(res.getId())
                                            .entityId(user).refType(GlobalConstants.ROLE_TYPE_USER).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build());
                                } else {
                                    userRef.add(RoleResourceRef.builder().roleId(finalFolibRole.getId()).resourceId(res.getId())
                                            .entityId(user).refType(GlobalConstants.ROLE_TYPE_USER).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                                }
                            });
                        });
                        userRef.add(RoleResourceRef.builder().roleId(roleId).resourceId(storageId).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                        roleResourceRefService.saveBath(userRef);
                    }
                });
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    @Override
    public void getStorageUsers(List<Storage> storages) {
        Set<String> storageIds = storages.stream().map(Storage::getId).collect(Collectors.toSet());
        Map<String, Set<String>> userMap = getStorageUser(storageIds);

        Map<String, List<PermissionsDTO>> repResolveMap = getRepositoryResolves(storages);

        // 根据资源查询角色关联的用户、用户组下的用户
        List<String> roleIds = storageIds.stream().flatMap(storageId -> Stream.of(String.format("STORAGE_ADMIN_%S", storageId))).collect(Collectors.toList());
        List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(roleIds);
        Map<String, List<RoleResourceRef>> roleUserMap = roleResourceRefs.stream().filter(r -> Objects.nonNull(r.getRefType()) && r.getRefType().equals(GlobalConstants.ROLE_TYPE_USER)).collect(Collectors.groupingBy(RoleResourceRef::getRoleId));
        storages.forEach(storage -> {
            storage.setUsers(new HashSet<>());
            List<RoleResourceRef> adminRef = roleUserMap.get(String.format("STORAGE_ADMIN_%S", storage.getId()));
            if (CollectionUtils.isNotEmpty(adminRef)) {
                String adminUser = adminRef.get(0).getEntityId();
                storage.setAdmin(adminUser);
                storage.setUsers(Collections.singleton(adminUser));
            }
            Set<String> storageUsers = userMap.get(storage.getId());
            if (CollectionUtils.isNotEmpty(storageUsers)) {
                storageUsers.addAll(storage.getUsers());
                storage.setUsers(storageUsers);
            }
            //判断存储空间下仓库有没有下载权限
            List<PermissionsDTO> permissionsDTOS = repResolveMap.get(storage.getId());
            if(CollectionUtils.isNotEmpty(permissionsDTOS)) {
                List<String> userIds = getUserIds(permissionsDTOS);
                userIds.removeAll(storage.getUsers());
                if (CollectionUtils.isNotEmpty(userIds)) {
                    storage.setRepositoryUsers((new HashSet<>(userIds)));
                    storage.getUsers().addAll(userIds);
                }
            }
        });
    }

    @Override
    public long getStorageCount() {
        if(configurationManagementService.getConfiguration() !=null && !configurationManagementService.getConfiguration().getStorages().isEmpty()){
            return configurationManagementService.getConfiguration().getStorages().size();
        }
        return 0;
    }

    private Map<String, List<PermissionsDTO>> getRepositoryResolves(List<Storage> storages) {
        List<String> repResourceIds = new ArrayList<>();
        storages.forEach(storage -> {
            String storageId = storage.getId();
            Collection<? extends Repository> values = storage.getRepositories().values();
            values.forEach(
                    repository -> {
                        String repositoryId = repository.getId();
                        repResourceIds.add((storageId + "_" + repositoryId).toUpperCase());
                    }
            );
        });
        //查询有规则的资源角色信息
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissionsByResourceIds(repResourceIds);
        //查询用户的资源角色信息
        String username = UserUtils.getUsername();
        if (StringUtils.isNotBlank(username)) {
            List<PermissionsDTO> roleResourceRefs = roleResourceRefService.queryPermissions(null, username, null, null, false);
            if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
                permissions.addAll(roleResourceRefs);
            }
        }
        List<PermissionsDTO> repResolveList = permissions.stream().filter(permission -> Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getRepositoryPrivilege()) || Privileges.ARTIFACTS_RESOLVE.name().equals(permission.getPathPrivilege())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(repResolveList)) {
            return Collections.emptyMap();
        }
        return repResolveList.stream().filter(permission -> StringUtils.isNotEmpty(permission.getStorageId())).collect(Collectors.groupingBy(PermissionsDTO::getStorageId));
    }

    private List<String> getUserIds(List<PermissionsDTO> permissionsDTOS) {
        List<String> userIds = permissionsDTOS.stream().filter(permission -> GlobalConstants.ROLE_TYPE_USER.equals(permission.getRefType())).map(PermissionsDTO::getEntityId).distinct().collect(Collectors.toList());
        List<Long> groupIds = permissionsDTOS.stream().filter(permission -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(permission.getRefType())).map(p -> Long.valueOf(p.getEntityId())).distinct().collect(Collectors.toList());
        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
        List<String> userIdList = userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList());
        userIds.addAll(userIdList);
        return userIds;
    }

    /**
     *
     * 方法描述:  根据存储空间id查询id关联的用户，查询存储空间id关联的用户组下的用户
     *
     * @param: storageIds 存储空间id集合
     * @return:  Map<String, Set<String>> 存储空间id为key的用户集合
     */
    @Override
    public Map<String, Set<String>> getStorageUser(Set<String> storageIds) {
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissionsByStorageIds(new ArrayList<>(storageIds));
        Map<String, Set<String>> userMap = permissions.stream().filter(p -> GlobalConstants.ROLE_TYPE_USER.equals(p.getRefType()) && Privileges.ARTIFACTS_RESOLVE.name().equalsIgnoreCase(p.getStoragePrivilege())).collect(Collectors.groupingBy(PermissionsDTO::getStorageId,
                Collectors.mapping(PermissionsDTO::getEntityId, Collectors.toSet())));
        //查询用户组关联的用户
        Map<String, Set<String>> groupMap = permissions.stream().filter(p -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(p.getRefType())  && Privileges.
                ARTIFACTS_RESOLVE.name().equalsIgnoreCase(p.getStoragePrivilege())).collect(Collectors.groupingBy(PermissionsDTO::getStorageId,
                Collectors.mapping(PermissionsDTO::getEntityId, Collectors.toSet())));
        List<Long> groupIds = groupMap.values().stream().flatMap(Set::stream).map(Long::valueOf).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(groupIds)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            Map<Long, Set<String>> groupUserMap = userGroupRefs.stream().collect(Collectors.groupingBy(UserGroupRef::getUserGroupId, Collectors.mapping(UserGroupRef::getUserId, Collectors.toSet())));
            if (!groupUserMap.isEmpty()){
                groupUserMap.forEach((groupId, users) -> {
                    if (userMap.containsKey(groupId.toString())){
                        userMap.get(groupId.toString()).addAll(users);
                    }else {
                        userMap.put(groupId.toString(), users);
                    }
                });
            }
        }

        //查询关联下载权限的角色
        List<String> roleIds = Arrays.asList(SystemRole.ADMIN.name(), SystemRole.READERS.name(), SystemRole.ANONYMOUS.name());
        List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryByRoleIds(roleIds);
        //查询下载权限角色关联的用户
        List<String> resolveUserIds = Optional.ofNullable(roleResourceRefs).orElse(Collections.emptyList()).stream().filter(r -> StringUtils.isNotEmpty(r.getRefType()) && GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).distinct().collect(Collectors.toList());

        //查询下载权限角色关联的用户组
        List<Long> resolveGroupIds = Optional.ofNullable(roleResourceRefs).orElse(Collections.emptyList()).stream().filter(r -> StringUtils.isNotEmpty(r.getRefType()) && GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(RoleResourceRef::getEntityId).distinct().map(Long::valueOf).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(resolveGroupIds)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(resolveGroupIds);
            Set<String> collect = Optional.ofNullable(userGroupRefs).orElse(Collections.emptyList()).stream().map(UserGroupRef::getUserId).collect(Collectors.toSet());
            resolveUserIds.addAll(collect);
        }

        if (CollectionUtils.isNotEmpty(resolveUserIds)){
            storageIds.forEach(storageId -> {
                if (userMap.containsKey(storageId)) {
                    userMap.get(storageId).addAll(resolveUserIds);
                } else {
                    userMap.put(storageId, new HashSet<>(resolveUserIds));
                }
            });
        }

        return userMap;
    }

    /**
     * 处理原有的管理员角色
     *
     * @param username  用户名
     * @param storageId 存储空间名称
     */
    private void handlerOriginalStorageAdminRole(String username, String storageId) {
        Storage originalStorage = configurationManagementService.getConfiguration().getStorage(storageId);
        String originalUsername = originalStorage.getAdmin();
        if (StringUtils.isNotBlank(originalUsername) && !originalUsername.equals(username)) {
            logger.info("storageId {} manager change {} to {}", storageId, originalStorage.getAdmin(), username);
            //管理员变更
            Set<String> storageIdSet = getManagerStorageIdList(originalUsername);
            storageIdSet.remove(storageId);
            handlerStorageAdminRole(originalUsername, null, storageIdSet);
        }
    }

    /**
     * 维护数据库中的存储空间管理员角色
     *
     * @param username
     * @param storageId
     */
    private void handlerOriginalStorageAdminRoleByDB(String username, String storageId) {
        String key = String.format("STORAGE_ADMIN_%S", storageId);
        List<RoleResourceRef> roleAdminRefs = roleResourceRefService.queryRefs(RoleResourceRef.builder().roleId(key).refType(GlobalConstants.ROLE_TYPE_USER).build());
        if (CollectionUtils.isNotEmpty(roleAdminRefs)) {
            List<String> roleAdmins = roleAdminRefs.stream().map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            if (!roleAdmins.contains(username)) {
                roleResourceRefService.removeByIds(roleAdminRefs.stream().map(RoleResourceRef::getId).collect(Collectors.toList()));
            }
        }
    }

    /**
     * 处理存储空间管理员角色
     *
     * @param username         用户名
     * @param currentStorageId 存储空间名称
     */
    private void handlerStorageAdminRoleByDB(String username, String currentStorageId) {
        if (StringUtils.isNotBlank(username)) {
            try {

                String key = String.format("STORAGE_ADMIN_%S", currentStorageId);
                FolibRole folibRole = folibRoleService.queryById(key);
                if (folibRole == null) {
                    folibRoleService.insert(FolibRole.builder().id(key).cnName(String.format("存储空间%s管理员", currentStorageId)).enName(key).deleted(GlobalConstants.NOT_DELETED).isDefault(GlobalConstants.NOT_DEFAULT).description("存储空间管理员的专属角色").build());
                }
                String resourceId = currentStorageId.toUpperCase();
                Resource resource = resourceService.queryById(currentStorageId.toUpperCase());
                if (resource == null) {
                    Resource storageResource = Resource.builder().id(currentStorageId.toUpperCase()).storageId(currentStorageId).build();
                    resourceService.insert(storageResource);
                }
                Set<String> privileges = privileges();
                List<RoleResourceRef> roleResourceRefs = privileges.stream().map(privilege -> RoleResourceRef.builder().roleId(key).entityId(username).refType(GlobalConstants.ROLE_TYPE_USER).resourceId(resourceId)
                        .storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
                roleResourceRefs.add(RoleResourceRef.builder().roleId(key).resourceId(resourceId).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                roleResourceRefService.saveBath(roleResourceRefs);

            } catch (Exception ex) {
                logger.error("handler user {} storage {} admin role error：{}", username, currentStorageId, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex);
            }
        }
    }

    /**
     * 处理存储空间普通角色
     *
     * @param users         用户名
     * @param currentStorageId 存储空间名称
     */
    private void handlerStorageOrdinaryRoleByDB(Set<String> users, String currentStorageId) {
        String key = String.format("STORAGE_USER_%S", currentStorageId);
        FolibRole folibRole = folibRoleService.queryById(key);
        if (folibRole == null) {
            folibRole = FolibRole.builder()
                    .id(key)
                    .cnName(String.format("存储空间%s用户", currentStorageId))
                    .enName(String.format("STORAGE_USER_%S", currentStorageId))
                    .isDefault(GlobalConstants.NOT_DEFAULT).deleted(GlobalConstants.NOT_DELETED)
                    .description(String.format("存储空间%s下的普通用户", currentStorageId))
                    .build();
            folibRoleService.insert(folibRole);
        }
        Resource storageResource = Resource.builder().id(currentStorageId.toUpperCase()).storageId(currentStorageId).build();
        Resource resource = resourceService.queryResource(storageResource);
        if (resource == null) {
            resourceService.insert(storageResource);
        }
        if (CollectionUtils.isNotEmpty(users)) {
            try {
                List<RoleResourceRef> roleResourceRefs = new ArrayList<>();
                users.forEach(user -> roleResourceRefs.add(RoleResourceRef.builder().roleId(key).entityId(user).refType(GlobalConstants.ROLE_TYPE_USER).resourceId(resource.getId()).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()));
                if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
                    roleResourceRefService.saveBath(roleResourceRefs);
                }

            } catch (Exception ex) {
                logger.error("handler user {} storage {} admin role error：{}", users, currentStorageId, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }else {
            roleResourceRefService.insert(RoleResourceRef.builder().roleId(key).resourceId(storageResource.getId()).build());
        }
    }

    /**
     * 处理存储空间管理员角色
     *
     * @param username         用户名
     * @param currentStorageId 存储空间名称
     * @param storageIdSet     指定存储空间
     */
    private void handlerStorageAdminRole(String username, String currentStorageId, Set<String> storageIdSet) {
        if (StringUtils.isNotBlank(username) && !isAdmin(username)) {
            if (Objects.isNull(storageIdSet)) {
                storageIdSet = getManagerStorageIdList(username);
            }
            if (StringUtils.isNotBlank(currentStorageId)) {
                storageIdSet.add(currentStorageId);
            }
            logger.info("{} manager storage：{}", username, storageIdSet);
            RepositoryAccessModel repositoryAccessModelForm;
            String storageRoleName = String.format("%s-%s", "STORAGE", username.toUpperCase());
            AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
            List<RoleDto> roles = authorizationConfigDto.getRoles().stream().filter(item -> item.getName().equals(storageRoleName)).collect(Collectors.toList());
            try {
                if (CollectionUtils.isNotEmpty(roles)) {
                    //存储空间角色已存在，先删除
                    authorizationConfigService.deleteRole(storageRoleName);
                    User user = userInfo(username);
                    if (Objects.isNull(user)) {
                        return;
                    }
                    SecurityRoleEntity securityRole = new SecurityRoleEntity();
                    securityRole.setUuid(storageRoleName);
                    user.getRoles().remove(securityRole);
                    userService.saveOverrideRole(user);
                }
                if (CollectionUtils.isEmpty(storageIdSet)) {
                    return;
                }
                //普通用户，对于其管理的存储空间赋予权限
                RoleModel roleModel = new RoleModel();
                roleModel.setName(storageRoleName);
                String description = String.format("【%s】作为存储空间管理员的专属角色", username);
                roleModel.setDescription(description);
                AccessModel accessModelModelForm = new AccessModel();
                for (String storageId : storageIdSet) {
                    repositoryAccessModelForm = new RepositoryAccessModel();
                    repositoryAccessModelForm.setStorageId(storageId);
                    repositoryAccessModelForm.setPrivileges(privileges());
                    accessModelModelForm.addRepositoryAccess(repositoryAccessModelForm);
                }
                roleModel.setAccessModel(accessModelModelForm);
                RoleDto role = RoleModelToRoleConverter.convert(roleModel);
                authorizationConfigService.addRole(role);
                User user = userInfo(username);
                if (Objects.isNull(user)) {
                    return;
                }
                SecurityRoleEntity securityRole = new SecurityRoleEntity();
                securityRole.setUuid(storageRoleName);
                user.getRoles().add(securityRole);
                userService.saveOverrideRole(user);
            } catch (Exception ex) {
                logger.error("handler user {} storage {} admin role error：{}", username, currentStorageId, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
    }

    /**
     * 获取用户信息
     *
     * @param username 用户名
     * @return 用户信息
     */
    private User userInfo(String username) {
        return userService.findByUsername(username);
    }

    /**
     * 判断是否有管理员角色
     *
     * @return true 是 false 否
     */
    private boolean isAdmin(String username) {
        User user = userInfo(username);
        if (Objects.isNull(user)) {
            return false;
        }
        return user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()).contains(SystemRole.ADMIN.name());
    }

    /**
     * 获取用户管理的存储空间
     *
     * @param username 用户名
     * @return 存储空间列表
     */
    private Set<String> getManagerStorageIdList(String username) {
        return Optional.of(configurationManagementService.getConfiguration().getStorages().values()).orElse(Lists.newArrayList()).stream()
                .filter(item -> username.equals(item.getAdmin())).map(Storage::getId).collect(Collectors.toSet());
    }

    /**
     * 获取仓库名称
     *
     * @param storageId 存储空间名称
     * @return 仓库名称列表
     */
    private Set<String> getRepositoryIdList(String storageId) {
        return configurationManagementService.getConfiguration().getStorage(storageId).getRepositories().keySet();
    }

    /**
     * 获取存储空间管理员应该具有的权限信息
     *
     * @return 权限信息
     */
    private Set<String> privileges() {
        EnumSet<Privileges> storagePrivileges = Privileges.storageAll();
        Set<String> privileges = storagePrivileges.stream().map(Privileges::getAuthority).collect(Collectors.toSet());
        logger.info("storage admin privileges：{}", privileges);
        return privileges;
    }

}

