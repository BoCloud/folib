package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.authorization.domain.RoleData;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converters.RoleModelToRoleConverter;
import com.veadan.folib.domain.*;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.enums.StorageProviderEnum;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.StoragePrivileges;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author mtodorov
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

    @Override
    public void updateStorage(StorageDto storage)
            throws IOException {
        handlerOriginalStorageAdminRole(storage.getAdmin(), storage.getId());
        configurationManagementService.updateStorage(storage);
        handlerStorageAdminRole(storage.getAdmin(), storage.getId(), null);
    }

    @Override
    public void createStorage(StorageDto storage)
            throws IOException {
        configurationManagementService.createStorage(storage);
        handlerStorageAdminRole(storage.getAdmin(), storage.getId(), null);
    }

    @Override
    public void removeStorage(String storageId)
            throws IOException {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        for (Repository repository : storage.getRepositories().values()) {
            repositoryManagementService.removeRepository(storageId, repository.getId());
        }
        handlerStorageAdminRole(storage.getAdmin(), "", null);
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
        if(CollectionUtils.isNotEmpty(storages)) {
            storages.forEach(storage -> {
                Set<String> users = storage.getUsers();
                //添加用户和资源绑定
                if(CollectionUtils.isNotEmpty(users)) {
                    String storageId = storage.getId();
                    Resource resoucreParam = Resource.builder().storageId(storageId).build();
                    //添加资源
                    Resource resource = resourceService.queryResource(resoucreParam);
                    if(Objects.isNull(resource)) {
                        resource = resourceService.insert(resoucreParam);
                    }
                    //创建角色
                    String roleId = String.format("storage_%s_user", storageId);
                    FolibRole folibRole = folibRoleService.queryById(roleId);
                    if(Objects.isNull(folibRole)) {
                        folibRole = FolibRole.builder()
                                .id(roleId)
                                .cnName(String.format("存储空间%s用户", storageId))
                                .enName(String.format("storage_%s_user", storageId))
                                .isDefault(GlobalConstants.NOT_DEFALUT).deleted(GlobalConstants.NOT_DELETED)
                                .description(String.format("存储空间%s下的普通用户", storageId))
                                .build();
                        folibRole = folibRoleService.insert(folibRole);
                    }
                    List<RoleResourceRef> userRef = new ArrayList<>(users.size());
                    FolibRole finalFolibRole = folibRole;
                    Resource finalResource = resource;
                    users.forEach(user -> {
                        userRef.add(RoleResourceRef.builder().roleId(finalFolibRole.getId()).resourceId(finalResource.getId())
                                .entityId(user).refType(GlobalConstants.ROLE_TYPE_USER).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                    });
                    roleResourceRefService.saveBath(userRef);
                }
            });
        }
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
        Set<RoleData> roleDataSet = authorizationConfigService.get().getRoles();
        List<String> roleNameList = Lists.newArrayList("GLOBAL_CONFIGURATION_MANAGER", "REPOSITORY_MANAGER", "ARTIFACTS_MANAGER");
        Set<String> privileges = Sets.newHashSet();
        for (RoleData roleData : roleDataSet) {
            if (roleNameList.contains(roleData.getName()) && Objects.nonNull(roleData.getAccessModel()) && CollectionUtils.isNotEmpty(roleData.getAccessModel().getApiAuthorities())) {
                privileges.addAll(roleData.getAccessModel().getApiAuthorities().stream().map(Privileges::getAuthority).collect(Collectors.toSet()));
            }
        }
        logger.info("storage admin privileges：{}", privileges);
        return privileges;
    }

}

