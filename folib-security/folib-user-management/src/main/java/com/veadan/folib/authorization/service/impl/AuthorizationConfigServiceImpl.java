package com.veadan.folib.authorization.service.impl;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.authorization.domain.Client;
import com.veadan.folib.dto.PermissionsDTO;
import com.veadan.folib.users.dto.AccessModelDto;
import com.veadan.folib.users.dto.RepositoryPrivilegesDto;
import com.veadan.folib.users.dto.StoragePrivilegesDto;
import com.veadan.folib.users.service.RoleResourceRefService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.domain.AuthorizationConfig;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;


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
                         AuthorizationConfigServiceImpl.this.authorizationConfig = getAuthorizationConfigDto(null);
                     },
                     false);
    }

    private @NotNull AuthorizationConfigDto getAuthorizationConfigDto(String username) {
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(null, username);
        Map<String, List<PermissionsDTO>> permissionMap = permissions.stream().filter(dto -> dto.getRoleId() != null).collect(Collectors.groupingBy(PermissionsDTO::getRoleId, Collectors.toList()));
        AuthorizationConfigDto authorizationConfig = new AuthorizationConfigDto();
        Set<RoleDto> roles = new LinkedHashSet<>();

        permissionMap.keySet().forEach(roleId -> {
            RoleDto roleDto = new RoleDto();
            roleDto.setName(roleId);
            List<PermissionsDTO> permissionsDTOS = permissionMap.get(roleId);
            roleDto.setDescription(permissionsDTOS.get(0).getDescription());
            AccessModelDto accessModel = new AccessModelDto();
            accessModel.setApiAuthorities(permissionsDTOS.stream().filter(dto -> dto.getApiAuthoritie() != null).map(dto -> Privileges.valueOf(dto.getApiAuthoritie())).collect(Collectors.toSet()));
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
                    repositoryPrivilege.setRepositoryPrivileges(repositoryMap.get(repositoryId).stream().filter(dto -> dto.getRepositoryPrivilege() != null).map(dto -> Privileges.valueOf(dto.getRepositoryPrivilege())).collect(Collectors.toSet()));
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
            return new AuthorizationConfig(getAuthorizationConfigDto(null));
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
            return new AuthorizationConfig(getAuthorizationConfigDto(username));
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
    public void addPrivilegesToAnonymous(final List<Privileges> privilegeList) throws IOException
    {
        modifyInLock(config ->
                     {
                         Set<RoleDto> roles = config.getRoles();
                         roles.stream()
                              .filter(r -> r.getName()
                                            .equalsIgnoreCase(SystemRole.ANONYMOUS.name()))
                              .findFirst()
                              .ifPresent(r -> privilegeList.stream()
                                                           .forEach(p -> r.addPrivilege(p)));
                     });
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
        modifyInLock(config ->
        {
            Set<RoleDto> roles = config.getRoles();
            roles.stream()
                    .filter(r -> r.getName()
                            .equalsIgnoreCase(SystemRole.ANONYMOUS.name()))
                    .findFirst()
                    .ifPresent(r -> r.getAccessModel().getApiAuthorities().clear());
        });
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

            if (storeInFile)
            {
                authorizationConfigFileManager.store(authorizationConfig);
            }
        }
        finally
        {
            writeLock.unlock();
        }
    }

}
