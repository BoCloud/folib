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
package com.folib.authorization.service.impl;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.authorization.AuthorizationConfigFileManager;
import com.folib.authorization.domain.AuthorizationConfig;
import com.folib.authorization.domain.Client;
import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.RoleDto;
import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.constant.GlobalConstants;
import com.folib.dto.PermissionsDTO;
import com.folib.entity.RoleResourceRef;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.dto.AccessModelDto;
import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.StoragePrivilegesDto;
import com.folib.users.service.RoleResourceRefService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.stream.Collectors;


/**
 * @author
 * @author veadan
 */
@Slf4j
@Service
public class AuthorizationConfigServiceImpl
        implements AuthorizationConfigService
{

    private final ReadWriteLock authorizationConfigLock = new ReentrantReadWriteLock();

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;

    /**
     * Yes, this is a state object.
     * It is protected by the {@link #authorizationConfigLock} here
     * and should not be exposed to the world.
     */
    private AuthorizationConfigDto authorizationConfig;
    @Inject
    private RoleResourceRefService roleResourceRefService;

    @Override
    public void setAuthorizationConfig(final AuthorizationConfigDto newConfig) throws IOException
    {
        modifyInLock(config ->
                     {
                         AuthorizationConfigServiceImpl.this.authorizationConfig = getAuthorizationConfigDto(null,null);
                     },
                     false);
    }
    public AuthorizationConfig getRole(String roleId)
    {
        return new AuthorizationConfig(getAuthorizationConfigDto(roleId, null));
    }

    private AuthorizationConfigDto getAuthorizationConfigDto(String roleName, String username) {
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(roleName, username, null, null, false);
        Map<String, List<PermissionsDTO>> permissionMap = permissions.stream().filter(dto -> dto.getRoleId() != null).collect(Collectors.groupingBy(PermissionsDTO::getRoleId, Collectors.toList()));
        Map<String, List<RoleResourceRef>> apiMap = new HashMap<>();
        if (!permissionMap.isEmpty()) {
            List<String> roleIds = permissions.stream().map(PermissionsDTO::getRoleId).distinct().collect(Collectors.toList());
            List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryApiAuthorities(roleIds);
            if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
                apiMap = roleResourceRefs.stream().filter(api -> StrUtil.isNotEmpty(api.getApiAuthoritie())).collect(Collectors.groupingBy(RoleResourceRef::getRoleId));
            }
        } else {
            apiMap = new HashMap<>();
        }
        AuthorizationConfigDto authorizationConfig = new AuthorizationConfigDto();
        Set<RoleDto> roles = new LinkedHashSet<>();

        Map<String, List<RoleResourceRef>> finalApiMap = apiMap;
        permissionMap.keySet().forEach(roleId -> {
            RoleDto roleDto = new RoleDto();
            roleDto.setName(roleId);
            List<PermissionsDTO> permissionsDTOS = permissionMap.get(roleId);
            roleDto.setDescription(permissionsDTOS.get(0).getDescription());
            AccessModelDto accessModel = new AccessModelDto();
            List<RoleResourceRef> apiRefs = finalApiMap.get(roleId);
            if(apiRefs != null) {
                accessModel.setApiAuthorities(apiRefs.stream().filter(dto -> dto.getApiAuthoritie() != null).map(dto -> Privileges.valueOf(dto.getApiAuthoritie())).collect(Collectors.toSet()));
            }
            Set<StoragePrivilegesDto> storageAuthorities = new LinkedHashSet<>();
            Map<String, List<PermissionsDTO>> storageMap = permissionsDTOS.stream().distinct().filter(dto -> dto.getStorageId() != null).collect(Collectors.groupingBy(PermissionsDTO::getStorageId, Collectors.toList()));
            storageMap.keySet().forEach(storageId -> {
                StoragePrivilegesDto storagePrivileges = new StoragePrivilegesDto();
                storagePrivileges.setStorageId(storageId);
                storagePrivileges.setStoragePrivileges(storageMap.get(storageId).stream().filter(dto -> dto.getStoragePrivilege() != null).map(dto -> Privileges.valueOf(dto.getStoragePrivilege())).collect(Collectors.toSet()));
                Set<RepositoryPrivilegesDto> repositoryPrivileges = new LinkedHashSet<>();
                Map<String, List<PermissionsDTO>> repositoryMap = storageMap.get(storageId).stream().filter(dto -> dto.getRepositoryId() != null).collect(Collectors.groupingBy(PermissionsDTO::getRepositoryId, Collectors.toList()));
                repositoryMap.keySet().forEach(repositoryId -> {
                    RepositoryPrivilegesDto repositoryPrivilege = new RepositoryPrivilegesDto();
                    repositoryPrivilege.setRepositoryId(repositoryId);
                    List<PermissionsDTO> repositorys = repositoryMap.get(repositoryId);
                    Set<Privileges> repositoryPrivilegeList = repositorys.stream().filter(dto -> dto.getRepositoryPrivilege() != null).map(dto -> Privileges.valueOf(dto.getRepositoryPrivilege())).collect(Collectors.toSet());
                    repositoryPrivilege.setRepositoryPrivileges(repositoryPrivilegeList);

                    Map<String, Set<PermissionsDTO>> pathMap = repositorys.stream().filter(dto -> StringUtils.isNotEmpty(dto.getPath())).collect(Collectors.groupingBy(PermissionsDTO::getPath, Collectors.toSet()));
                    if (!pathMap.isEmpty()) {
                        Set<PathPrivilegesDto> pathPrivilegesDtos = new HashSet<>();
                        pathMap.forEach((path, pathPrivileges) -> {
                            if (pathPrivileges != null) {
                                Set<Privileges> privilegesSet = pathPrivileges.stream()
                                        .filter(dto -> dto.getPathPrivilege() != null)
                                        .map(dto -> Privileges.valueOf(dto.getPathPrivilege()))
                                        .collect(Collectors.toSet());

                                PathPrivilegesDto pathPrivilegesDto = PathPrivilegesDto.builder()
                                        .path(path)
                                        .privileges(privilegesSet)
                                        .build();

                                pathPrivilegesDtos.add(pathPrivilegesDto);
                            }
                        });
                        repositoryPrivilege.setPathPrivileges(pathPrivilegesDtos);
                    }
                    repositoryPrivileges.add(repositoryPrivilege);
                });
                storagePrivileges.setRepositoryPrivileges(repositoryPrivileges);
                storageAuthorities.add(storagePrivileges);
            });
            accessModel.setStorageAuthorities(storageAuthorities);
            roleDto.setAccessModel(accessModel);
            roles.add(roleDto);
        });
        authorizationConfig.setRoles(roles);
        return authorizationConfig;
    }

    @Override
    public AuthorizationConfigDto getDto()
    {
        final Lock readLock = authorizationConfigLock.readLock();
        readLock.lock();

        try
        {
            return SerializationUtils.clone(authorizationConfig);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public AuthorizationConfig get()
    {
        final Lock readLock = authorizationConfigLock.readLock();
        readLock.lock();

        try
        {
            return new AuthorizationConfig(getAuthorizationConfigDto(null, null));
        }
        finally
        {
            readLock.unlock();
        }
    }

    public AuthorizationConfig get(String username)
    {
        final Lock readLock = authorizationConfigLock.readLock();
        readLock.lock();

        try
        {
            return new AuthorizationConfig(getAuthorizationConfigDto(null, username));
        }
        finally
        {
            readLock.unlock();
        }
    }
    @Override
    public void addRole(final RoleDto role) throws IOException
    {
        modifyInLock(config ->
                     {
                         config.getRoles().add(role);
                     });
    }

    public void addClient(final Client client) throws IOException
    {
        modifyInLock(config ->
        {
            config.getClients().add(client);
        });
    }

    public boolean deleteClient(final String clientId) throws IOException
    {
        MutableBoolean result = new MutableBoolean();
        modifyInLock(config ->
        {
            Set<Client> clients = config.getClients();
            clients.stream()
                    .filter(r -> r.getClientId()
                            .equalsIgnoreCase(clientId))
                    .findFirst()
                    .ifPresent(r -> {
                        result.setValue(clients.remove(r));}
                            );
        });
        return result.isTrue();
    }



    @Override
    public boolean deleteRole(final String roleName) throws IOException
    {
        MutableBoolean result = new MutableBoolean();
        modifyInLock(config ->
                     {
                         Set<RoleDto> roles = config.getRoles();
                         roles.stream()
                              .filter(r -> r.getName()
                                            .equalsIgnoreCase(roleName))
                              .findFirst()
                              .ifPresent(r -> result.setValue(roles.remove(r)));
                     });
        return result.isTrue();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addPrivilegesToAnonymous(final List<Privileges> privilegeList) throws IOException
    {
        if (CollectionUtils.isNotEmpty(privilegeList)) {
            List<RoleResourceRef> anonymousPrivileges = privilegeList.stream().map(privileges ->
                    RoleResourceRef.builder().roleId(SystemRole.ANONYMOUS.name()).resourceId(privileges.name().toUpperCase()).resourceType(GlobalConstants.RESOURCE_TYPE_API).build()
            ).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(anonymousPrivileges)) {
                clearPrivilegesAnonymous();
                roleResourceRefService.saveBath(anonymousPrivileges);
            }
        }
    }

    @Override
    public void handlerRole(String roleInfo) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            RoleDto role = objectMapper.readValue(roleInfo, RoleDto.class);
            if (getDto().getRoles().stream().anyMatch(item -> item.getName().equals(role.getName()))) {
                deleteRole(role.getName());
            }
            addRole(role);
        } catch (Exception ex) {
            log.error("处理角色信息 [{}] 失败 [{}]", roleInfo, ExceptionUtils.getStackTrace(ex));
        }
    }
    
    @Override
    public void clearPrivilegesAnonymous() throws IOException {
        roleResourceRefService.deleteAnonymousRole(SystemRole.ANONYMOUS.name());
    }

    private void modifyInLock(final Consumer<AuthorizationConfigDto> operation) throws IOException
    {
        modifyInLock(operation, true);
    }

    private void modifyInLock(final Consumer<AuthorizationConfigDto> operation,
                              final boolean storeInFile) throws IOException
    {
        final Lock writeLock = authorizationConfigLock.writeLock();
        writeLock.lock();

        try
        {
            operation.accept(authorizationConfig);

         /*   if (storeInFile)
            {
                authorizationConfigFileManager.store(authorizationConfig);
            }*/
        }
        finally
        {
            writeLock.unlock();
        }
    }

}
