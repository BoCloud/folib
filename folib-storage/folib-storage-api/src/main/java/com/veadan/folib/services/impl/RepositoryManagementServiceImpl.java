package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.converters.RoleModelToRoleConverter;
import com.veadan.folib.domain.*;
import com.veadan.folib.event.Event;
import com.veadan.folib.event.RepositoryBasedEvent;
import com.veadan.folib.event.repository.RepositoryEvent;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.event.repository.RepositoryEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryPermissionDto;
import com.veadan.folib.storage.repository.RepositoryPermissionUserDto;
import com.veadan.folib.storage.validation.resource.ArtifactOperationsValidator;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author mtodorov
 */
@Component("repositoryManagementService")
public class RepositoryManagementServiceImpl
        implements RepositoryManagementService {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryManagementServiceImpl.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private ArtifactOperationsValidator artifactOperationsValidator;

    @Inject
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @DatabaseUserService.Database
    @Lazy
    private UserService userService;

    @Override
    public void createRepository(String storageId,
                                 String repositoryId)
            throws IOException, RepositoryManagementStrategyException {
        LayoutProvider provider = getLayoutProvider(storageId, repositoryId);
        if (provider != null) {
            provider.getRepositoryManagementStrategy().createRepository(storageId, repositoryId);
        } else {
            Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryId);

            logger.warn("Layout provider '{}' could not be resolved. " +
                            "Using generic implementation instead.",
                    repository.getLayout());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);

            if (!Files.exists(repositoryPath)) {
                logger.info("Creating directories for [{}/{}]...", repository.getStorage().getId(), repository.getId());
                repositoryPath.getFileSystem().createRootDirectory();
            }
        }

        RepositoryEvent event = new RepositoryEvent(storageId,
                repositoryId,
                RepositoryEventTypeEnum.EVENT_REPOSITORY_CREATED.getType());

        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void removeRepository(String storageId,
                                 String repositoryId)
            throws IOException {
        LayoutProvider provider = getLayoutProvider(storageId, repositoryId);
        provider.getRepositoryManagementStrategy().removeRepository(storageId, repositoryId);

        RepositoryEvent event = new RepositoryEvent(storageId,
                repositoryId,
                RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETED.getType());

        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void deleteTrash(String storageId, String repositoryId)
            throws IOException {
        artifactOperationsValidator.checkStorageExists(storageId);
        artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);

        try {
            final Storage storage = getStorage(storageId);
            final Repository repository = storage.getRepository(repositoryId);

            artifactOperationsValidator.checkAllowsDeletion(repository);


            RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryFiles.deleteTrash(repositoryPath);

            RepositoryEvent event = new RepositoryEvent(storageId,
                    repositoryId,
                    RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH.getType());

            repositoryEventListenerRegistry.dispatchEvent(event);
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void deleteTrash()
            throws ArtifactStorageException {
        try {
            for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet()) {
                Storage storage = entry.getValue();

                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values()) {
                    if (repository.isAllowsDeletion()) {
                        logger.info("Emptying trash for repository {}...", repository.getId());

                        deleteTrash(repository.getStorage().getId(), repository.getId());
                    } else {
                        logger.warn("Repository {} does not support removal of trash.", repository.getId());
                    }
                }
            }

            int type = RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH_FOR_ALL_REPOSITORIES.getType();
            RepositoryEvent event = new RepositoryEvent(null, null, type);

            repositoryEventListenerRegistry.dispatchEvent(event);
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undelete(RepositoryPath repositoryPath)
            throws IOException {
        artifactOperationsValidator.validate(repositoryPath);

        final Repository repository = repositoryPath.getRepository();

        artifactOperationsValidator.checkAllowsDeletion(repository);

        try {
            RepositoryFiles.undelete(repositoryPath);

            int type = RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH_FOR_ALL_REPOSITORIES.getType();
            Event event = new RepositoryBasedEvent<>(repositoryPath, type);

            repositoryEventListenerRegistry.dispatchEvent(event);
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undeleteTrash(String storageId, String repositoryId)
            throws IOException {
        artifactOperationsValidator.checkStorageExists(storageId);
        artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);

        try {
            final Storage storage = getStorage(storageId);
            final Repository repository = storage.getRepository(repositoryId);

            if (repository.isTrashEnabled()) {
                RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
                RepositoryFiles.undelete(repositoryPath);

                RepositoryEvent event = new RepositoryEvent(storageId,
                        repositoryId,
                        RepositoryEventTypeEnum.EVENT_REPOSITORY_UNDELETE_TRASH
                                .getType());

                repositoryEventListenerRegistry.dispatchEvent(event);
            }
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undeleteTrash()
            throws IOException {

        for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet()) {
            Storage storage = entry.getValue();

            final Map<String, ? extends Repository> repositories = storage.getRepositories();
            for (Repository repository : repositories.values()) {
                final String storageId = storage.getId();
                final String repositoryId = repository.getId();

                try {
                    if (repository.isTrashEnabled()) {
                        RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
                        RepositoryFiles.undelete(repositoryPath);
                    }
                } catch (IOException e) {
                    throw new ArtifactStorageException("Unable to undelete trash for storage " + storageId + " in repository " +
                            repositoryId, e);
                }
            }
        }

        RepositoryEvent event = new RepositoryEvent(null,
                null,
                RepositoryEventTypeEnum.EVENT_REPOSITORY_UNDELETE_TRASH_FOR_ALL_REPOSITORIES
                        .getType());
        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void putInService(String storageId,
                             String repositoryId) throws IOException {
        configurationManagementService.putInService(storageId, repositoryId);
    }

    @Override
    public void putOutOfService(String storageId,
                                String repositoryId) throws IOException {
        configurationManagementService.putOutOfService(storageId, repositoryId);
    }

    private LayoutProvider getLayoutProvider(String storageId,
                                             String repositoryId) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        return layoutProviderRegistry.getProvider(repository.getLayout());
    }

    @Override
    public Storage getStorage(String storageId) {
        return getConfiguration().getStorages().get(storageId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handlerRepositoryPermission(String storageId, String repositoryId, RepositoryPermissionDto repositoryPermissionDto) {
        List<RepositoryPermissionUserDto> userList = repositoryPermissionDto.getUserList();
        if (CollectionUtils.isEmpty(userList)) {
            return;
        }
        for (RepositoryPermissionUserDto repositoryPermissionUser : userList) {
            handlerRepositoryUserPermission(storageId, repositoryId, repositoryPermissionUser.getUsername(), repositoryPermissionUser.getPermissions());
        }
    }

    @Override
    public void deleteRepositoryPermission(String storageId, String repositoryId, String username, String permissions) {
        try {
            User user = userInfo(username);
            String repositoryDeployRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DEPLOY.getAuthority());
            if (permissions.contains(Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                //包含仓库上传角色，移除
                if (user.getRoles().stream().anyMatch(item -> item.getRoleName().equals(repositoryDeployRoleName))) {
                    SecurityRoleEntity securityRole = new SecurityRoleEntity();
                    securityRole.setUuid(repositoryDeployRoleName);
                    user.getRoles().remove(securityRole);
                }
            }
            String repositoryDeleteRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DELETE.getAuthority());
            if (permissions.contains(Privileges.ARTIFACTS_DELETE.getAuthority())) {
                //包含仓库删除角色，移除
                if (user.getRoles().stream().anyMatch(item -> item.getRoleName().equals(repositoryDeleteRoleName))) {
                    SecurityRoleEntity securityRole = new SecurityRoleEntity();
                    securityRole.setUuid(repositoryDeleteRoleName);
                    user.getRoles().remove(securityRole);
                }
            }
            userService.saveOverrideRole(user);
        } catch (Exception ex) {
            logger.error("delete storage {} repository {} user {} permission error：{}", storageId, repositoryId, username, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

    /**
     * 处理仓库级别人员权限
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param username     用户名
     * @param permissions  权限
     */
    private void handlerRepositoryUserPermission(String storageId, String repositoryId, String username, List<String> permissions) {
        if (StringUtils.isNotBlank(username) && !isAdmin(username)) {
            try {
                User user = userInfo(username);
                AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
                String repositoryDeployRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DEPLOY.getAuthority());
                if (permissions.contains(Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                    //上传权限
                    boolean repositoryDeployRoleNameExists = authorizationConfigDto.getRoles().stream().anyMatch(item -> item.getName().equals(repositoryDeployRoleName));
                    if (!repositoryDeployRoleNameExists) {
                        //仓库deploy角色不存在，创建
                        createRole(repositoryDeployRoleName, storageId, repositoryId, Lists.newArrayList(Privileges.ARTIFACTS_DEPLOY.getAuthority()));
                    }
                    if (user.getRoles().stream().noneMatch(item -> item.getRoleName().equals(repositoryDeployRoleName))) {
                        SecurityRoleEntity securityRole = new SecurityRoleEntity();
                        securityRole.setUuid(repositoryDeployRoleName);
                        user.getRoles().add(securityRole);
                    }
                } else {
                    //包含仓库上传角色，移除
                    if (user.getRoles().stream().anyMatch(item -> item.getRoleName().equals(repositoryDeployRoleName))) {
                        SecurityRoleEntity securityRole = new SecurityRoleEntity();
                        securityRole.setUuid(repositoryDeployRoleName);
                        user.getRoles().remove(securityRole);
                    }
                }
                String repositoryDeleteRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DELETE.getAuthority());
                if (permissions.contains(Privileges.ARTIFACTS_DELETE.getAuthority())) {
                    //删除权限
                    boolean repositoryDeleteRoleNameExists = authorizationConfigDto.getRoles().stream().anyMatch(item -> item.getName().equals(repositoryDeleteRoleName));
                    if (!repositoryDeleteRoleNameExists) {
                        //仓库delete角色不存在，创建
                        createRole(repositoryDeleteRoleName, storageId, repositoryId, Lists.newArrayList(Privileges.ARTIFACTS_DELETE.getAuthority()));
                    }
                    if (user.getRoles().stream().noneMatch(item -> item.getRoleName().equals(repositoryDeleteRoleName))) {
                        SecurityRoleEntity securityRole = new SecurityRoleEntity();
                        securityRole.setUuid(repositoryDeleteRoleName);
                        user.getRoles().add(securityRole);
                    }
                } else {
                    //包含仓库删除角色，移除
                    if (user.getRoles().stream().anyMatch(item -> item.getRoleName().equals(repositoryDeleteRoleName))) {
                        SecurityRoleEntity securityRole = new SecurityRoleEntity();
                        securityRole.setUuid(repositoryDeleteRoleName);
                        user.getRoles().remove(securityRole);
                    }
                }
                userService.saveOverrideRole(user);
            } catch (Exception ex) {
                logger.error("handler storage {} repository {} user {} permission error：{}", storageId, repositoryId, username, ExceptionUtils.getStackTrace(ex));
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
        return user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()).contains(SystemRole.ADMIN.name());
    }

    /**
     * 创建角色
     *
     * @param roleName     角色名称
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param privileges   权限点
     * @throws IOException 异常
     */
    private void createRole(String roleName, String storageId, String repositoryId, List<String> privileges) throws IOException {
        RoleModel roleModel = new RoleModel();
        roleModel.setName(roleName);
        String description = String.format("【%s-%s】的权限", storageId.toUpperCase(), repositoryId.toUpperCase());
        roleModel.setDescription(description);
        AccessModel accessModelModelForm = new AccessModel();
        RepositoryAccessModel repositoryAccessModelForm = new RepositoryAccessModel();
        repositoryAccessModelForm.setStorageId(storageId);
        repositoryAccessModelForm.setRepositoryId(repositoryId);
        repositoryAccessModelForm.setPrivileges(privileges);
        accessModelModelForm.addRepositoryAccess(repositoryAccessModelForm);
        roleModel.setAccessModel(accessModelModelForm);
        RoleDto role = RoleModelToRoleConverter.convert(roleModel);
        authorizationConfigService.addRole(role);
    }

}
