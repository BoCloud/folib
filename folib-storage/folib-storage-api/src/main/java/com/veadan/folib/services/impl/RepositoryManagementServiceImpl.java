package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.converters.RoleModelToRoleConverter;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
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
import com.veadan.folib.storage.ArtifactResolutionException;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryPermissionDto;
import com.veadan.folib.storage.repository.RepositoryPermissionUserDto;
import com.veadan.folib.storage.validation.resource.ArtifactOperationsValidator;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.PathPrivilegesDto;
import com.veadan.folib.users.dto.RepositoryPrivilegesDto;
import com.veadan.folib.users.dto.StoragePrivilegesDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
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
import java.util.*;
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
    @Lazy
    private CronTaskConfigurationService cronTaskConfigurationService;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
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
    public void cleanupRepository(String storageId, String repositoryId) throws IOException {
        LayoutProvider provider = getLayoutProvider(storageId, repositoryId);
        provider.getRepositoryManagementStrategy().removeRepository(storageId, repositoryId);
    }

    @Override
    public void deleteTrash(String storageId, String repositoryId, String storageDay, Map<String, String> cleanupArtifactPathMap)
            throws IOException {
        artifactOperationsValidator.checkStorageExists(storageId);
        artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);

        try {
            final Storage storage = getStorage(storageId);
            final Repository repository = storage.getRepository(repositoryId);

            artifactOperationsValidator.checkAllowsDeletion(repository);


            RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryFiles.deleteTrash(repositoryPath, storageDay, cleanupArtifactPathMap);

            RepositoryEvent event = new RepositoryEvent(storageId,
                    repositoryId,
                    RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH.getType());

            repositoryEventListenerRegistry.dispatchEvent(event);
        } catch (IOException e) {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void deleteTrash(boolean checkTask, String storageDay, Map<String, String> cleanupArtifactPathMap)
            throws ArtifactStorageException {
        try {
            String jobClass = "com.veadan.folib.cron.jobs.ClearRepositoryTrashCronJob";
            for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet()) {
                Storage storage = entry.getValue();

                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values()) {
                    if (checkTask && existsRepositoryTask(storage.getId(), repository.getId(), jobClass)) {
                        logger.info("Emptying trash for repository {} exists custom cron task skip...", ConfigurationUtils.getStorageIdAndRepositoryId(storage.getId(), repository.getId()));
                        continue;
                    }
                    if (repository.isAllowsDeletion()) {
                        logger.info("Emptying trash for repository {}...", ConfigurationUtils.getStorageIdAndRepositoryId(storage.getId(), repository.getId()));

                        deleteTrash(repository.getStorage().getId(), repository.getId(), storageDay, cleanupArtifactPathMap);
                    } else {
                        logger.warn("Repository {} does not support removal of trash.", ConfigurationUtils.getStorageIdAndRepositoryId(storage.getId(), repository.getId()));
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
            handlerRepositoryUserPermission(storageId, repositoryId, repositoryPermissionUser.getUsername(), repositoryPermissionUser.getPermissions(), repositoryPermissionUser.getPaths());
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
            removeRepositoryRole(user, authorizationConfigService.getDto(), storageId, repositoryId);
            userService.saveOverrideRole(user);
        } catch (Exception ex) {
            logger.error("delete storage {} repository {} user {} permission error：{}", storageId, repositoryId, username, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    @Override
    public void deleteEmptyDirectory() {
        try {
            String jobClass = "com.veadan.folib.cron.jobs.ClearRepositoryEmptyDirectoryCronJob";
            for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet()) {
                Storage storage = entry.getValue();
                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values()) {
                    if (existsRepositoryTask(storage.getId(), repository.getId(), jobClass)) {
                        logger.info("Clear empty directory for repository {} exists custom cron task skip...", ConfigurationUtils.getStorageIdAndRepositoryId(storage.getId(), repository.getId()));
                        continue;
                    }
                    logger.info("Clear empty directory for repository {}...", ConfigurationUtils.getStorageIdAndRepositoryId(storage.getId(), repository.getId()));
                    deleteEmptyDirectory(repository.getStorage().getId(), repository.getId());
                }
            }
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public void deleteEmptyDirectory(String storageId, String repositoryId) {
        try {
            artifactOperationsValidator.checkStorageExists(storageId);
            artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);
            RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            RepositoryFiles.deleteEmptyDirectory(repositoryPath);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
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
     * @param paths        路径
     */
    private void handlerRepositoryUserPermission(String storageId, String repositoryId, String username, List<String> permissions, List<String> paths) {
        if (StringUtils.isNotBlank(username) && !isAdmin(username)) {
            try {
                User user = userInfo(username);
                AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
                String repositoryDeployRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DEPLOY.getAuthority());
                String repositoryDeleteRoleName = String.format("%s|%s|%s", storageId.toUpperCase(), repositoryId.toUpperCase(), Privileges.ARTIFACTS_DELETE.getAuthority());
                List<String> repositoryRoleNameList = Lists.newArrayList(repositoryDeployRoleName, repositoryDeleteRoleName);
                if (CollectionUtils.isEmpty(paths)) {
                    if (permissions.contains(Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                        //上传权限
                        boolean repositoryDeployRoleNameExists = authorizationConfigDto.getRoles().stream().anyMatch(item -> item.getName().equals(repositoryDeployRoleName));
                        if (!repositoryDeployRoleNameExists) {
                            //仓库deploy角色不存在，创建
                            createRole(repositoryDeployRoleName, "", storageId, repositoryId, Lists.newArrayList(Privileges.ARTIFACTS_DEPLOY.getAuthority()), paths);
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
                    if (permissions.contains(Privileges.ARTIFACTS_DELETE.getAuthority())) {
                        //删除权限
                        boolean repositoryDeleteRoleNameExists = authorizationConfigDto.getRoles().stream().anyMatch(item -> item.getName().equals(repositoryDeleteRoleName));
                        if (!repositoryDeleteRoleNameExists) {
                            //仓库delete角色不存在，创建
                            createRole(repositoryDeleteRoleName, "", storageId, repositoryId, Lists.newArrayList(Privileges.ARTIFACTS_DELETE.getAuthority()), paths);
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
                    removeRepositoryRole(user, authorizationConfigDto, storageId, repositoryId);
                } else {
                    String roleName = username.toUpperCase();
                    boolean roleNameExists = authorizationConfigDto.getRoles().stream().anyMatch(item -> item.getName().equals(roleName));
                    if (!roleNameExists) {
                        //仓库deploy角色不存在，创建
                        createRole(roleName, String.format("【%s】的权限", roleName), storageId, repositoryId, permissions, paths);
                    } else {
                        updateRole(authorizationConfigDto, roleName, storageId, repositoryId, permissions, paths);
                    }
                    user.getRoles().removeIf(item -> repositoryRoleNameList.contains(item.getRoleName()));
                    SecurityRoleEntity securityRole = new SecurityRoleEntity();
                    securityRole.setUuid(roleName);
                    user.getRoles().add(securityRole);
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
     * @param description  描述
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param privileges   权限点
     * @param paths        路径
     * @throws IOException 异常
     */
    private void createRole(String roleName, String description, String storageId, String repositoryId, List<String> privileges, List<String> paths) throws IOException {
        RoleModel roleModel = new RoleModel();
        roleModel.setName(roleName);
        if (StringUtils.isBlank(description)) {
            description = String.format("【%s-%s】的权限", storageId.toUpperCase(), repositoryId.toUpperCase());
        }
        roleModel.setDescription(description);
        AccessModel accessModelModelForm = new AccessModel();
        if (CollectionUtils.isEmpty(paths)) {
            RepositoryAccessModel repositoryAccessModelForm = new RepositoryAccessModel();
            repositoryAccessModelForm.setStorageId(storageId);
            repositoryAccessModelForm.setRepositoryId(repositoryId);
            repositoryAccessModelForm.setPrivileges(privileges);
            accessModelModelForm.addRepositoryAccess(repositoryAccessModelForm);
        } else {
            paths.forEach(path -> {
                if (StringUtils.isNotBlank(path)) {
                    path = StringUtils.removeEnd(path, "/");
                    RepositoryAccessModel repositoryAccessModelForm = new RepositoryAccessModel();
                    repositoryAccessModelForm.setStorageId(storageId);
                    repositoryAccessModelForm.setRepositoryId(repositoryId);
                    repositoryAccessModelForm.setPrivileges(privileges);
                    repositoryAccessModelForm.setPath(path);
                    accessModelModelForm.addRepositoryAccess(repositoryAccessModelForm);
                }
            });
        }
        roleModel.setAccessModel(accessModelModelForm);
        RoleDto role = RoleModelToRoleConverter.convert(roleModel);
        authorizationConfigService.addRole(role);
    }

    /**
     * 更新角色
     *
     * @param authorizationConfigDto 权限信息
     * @param roleName               角色名称
     * @param storageId              存储空间名称
     * @param repositoryId           仓库名称
     * @param privileges             权限点
     * @param paths                  路径
     * @throws IOException 异常
     */
    private void updateRole(AuthorizationConfigDto authorizationConfigDto, String roleName, String storageId, String repositoryId, List<String> privileges, List<String> paths) throws IOException {
        try {
            Optional<RoleDto> roleOptional = authorizationConfigDto.getRoles().stream().filter(item -> item.getName().equals(roleName)).findFirst();
            if (roleOptional.isEmpty()) {
                return;
            }
            RoleDto role = roleOptional.get();
            StoragePrivilegesDto storagePrivileges = null;
            RepositoryPrivilegesDto repositoryPrivileges = null;
            Set<Privileges> privilegeSet = Sets.newLinkedHashSet();
            if (privileges.contains(Privileges.ARTIFACTS_DEPLOY.getAuthority())) {
                privilegeSet.add(Privileges.ARTIFACTS_DEPLOY);
            }
            if (privileges.contains(Privileges.ARTIFACTS_DELETE.getAuthority())) {
                privilegeSet.add(Privileges.ARTIFACTS_DELETE);
            }
            Optional<StoragePrivilegesDto> storagePrivilegesOptional = role.getAccessModel().getStorageAuthorities(storageId);
            if (storagePrivilegesOptional.isPresent()) {
                storagePrivileges = storagePrivilegesOptional.get();
                Optional<RepositoryPrivilegesDto> repositoryPrivilegesOptional = storagePrivileges.getRepositoryPrivileges(repositoryId);
                if (repositoryPrivilegesOptional.isPresent()) {
                    repositoryPrivileges = repositoryPrivilegesOptional.get();
                }
            }
            if (Objects.isNull(storagePrivileges)) {
                storagePrivileges = new StoragePrivilegesDto();
                storagePrivileges.setStorageId(storageId);
                role.getAccessModel().getStorageAuthorities().add(storagePrivileges);
            }
            if (Objects.isNull(repositoryPrivileges)) {
                repositoryPrivileges = new RepositoryPrivilegesDto();
                repositoryPrivileges.setRepositoryId(repositoryId);
                storagePrivileges.getRepositoryPrivileges().add(repositoryPrivileges);
            }
            repositoryPrivileges.getPathPrivileges().clear();
            for (String path : paths) {
                if (StringUtils.isBlank(path)) {
                    continue;
                }
                path = StringUtils.removeEnd(path, "/");
                RepositoryAccessModel repositoryAccessModelForm = new RepositoryAccessModel();
                repositoryAccessModelForm.setStorageId(storageId);
                repositoryAccessModelForm.setRepositoryId(repositoryId);
                repositoryAccessModelForm.setPrivileges(privileges);
                repositoryAccessModelForm.setPath(path);
                PathPrivilegesDto pathPrivilegesDto = new PathPrivilegesDto();
                pathPrivilegesDto.setPath(path);
                pathPrivilegesDto.setPrivileges(privilegeSet);
                repositoryPrivileges.getPathPrivileges().add(pathPrivilegesDto);
            }
            authorizationConfigService.deleteRole(role.getName());
            authorizationConfigService.addRole(role);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * 移除仓库角色
     *
     * @param user                   用户
     * @param authorizationConfigDto 权限信息
     * @param storageId              存储空间名称
     * @param repositoryId           仓库名称
     */
    private void removeRepositoryRole(User user, AuthorizationConfigDto authorizationConfigDto, String storageId, String repositoryId) {
        try {
            String roleName = user.getUsername().toUpperCase();
            Optional<RoleDto> roleOptional = authorizationConfigDto.getRoles().stream().filter(item -> item.getName().equals(roleName)).findFirst();
            if (roleOptional.isEmpty()) {
                return;
            }
            RoleDto role = roleOptional.get();
            Optional<StoragePrivilegesDto> storagePrivilegesOptional = role.getAccessModel().getStorageAuthorities(storageId);
            if (storagePrivilegesOptional.isPresent()) {
                StoragePrivilegesDto storagePrivileges = storagePrivilegesOptional.get();
                Optional<RepositoryPrivilegesDto> repositoryPrivilegesOptional = storagePrivileges.getRepositoryPrivileges(repositoryId);
                if (repositoryPrivilegesOptional.isPresent()) {
                    storagePrivileges.getRepositoryPrivileges().removeIf(item -> item.getRepositoryId().equals(repositoryId));
                    if (CollectionUtils.isEmpty(storagePrivileges.getRepositoryPrivileges())) {
                        role.getAccessModel().getStorageAuthorities().removeIf(item -> item.getStorageId().equals(storageId));
                    }
                    authorizationConfigService.deleteRole(role.getName());
                    authorizationConfigService.addRole(role);
                }
            }
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    private boolean existsRepositoryTask(String storageId, String repositoryId, String jobClass) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && jobClass.equals(cron.getJobClass()));
    }

}
