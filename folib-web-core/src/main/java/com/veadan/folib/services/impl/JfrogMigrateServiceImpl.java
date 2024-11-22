package com.veadan.folib.services.impl;

import com.alibaba.excel.EasyExcel;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.converters.migrate.JfrogMigrateConvert;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.domain.adapter.jfrog.JfrogMapping;
import com.veadan.folib.domain.migrate.BatchChangeListener;
import com.veadan.folib.domain.migrate.BatchChangeRepository;
import com.veadan.folib.dto.AccessModelDTO;
import com.veadan.folib.dto.AccessResourcesDTO;
import com.veadan.folib.dto.AccessUserGroupsDTO;
import com.veadan.folib.dto.AccessUsersDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.enums.NotifyScopesTypeEnum;
import com.veadan.folib.enums.StorageProviderEnum;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.forms.JfrogMigrateForm;
import com.veadan.folib.mapper.UserGroupMapper;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.JfrogMigrateService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryDto;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.UserGroupService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.Repositories;
import org.jfrog.artifactory.client.model.Group;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.jfrog.artifactory.client.model.PermissionTarget;
import org.jfrog.artifactory.client.model.Principal;
import org.jfrog.artifactory.client.model.Principals;
import org.jfrog.artifactory.client.model.Privilege;
import org.jfrog.artifactory.client.model.RemoteRepository;
import org.jfrog.artifactory.client.model.User;
import org.jfrog.artifactory.client.model.VirtualRepository;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.LOCAL;
import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.REMOTE;
import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.VIRTUAL;

/**
 * @author huayanjun
 * @since 2024-10-22 17:01
 */

@Slf4j
@Service
public class JfrogMigrateServiceImpl extends BaseController implements JfrogMigrateService {

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Resource
    private UserGroupService userGroupService;

    @Resource
    private UserGroupMapper userGroupMapper;

    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;

    @Inject
    private PasswordEncoder passwordEncoder;
    @Resource
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;

    @Resource
    private RepositoryManagementService repositoryManagementService;
    @Resource
    private ClusterSyncService clusterSyncService;

    @Resource
    private StorageManagementService storageManagementService;

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private LayoutProviderRegistry layoutProviderRegistry;

    @Resource
    private ResourceService resourceService;

    @Resource
    private FolibRoleService folibRoleService;

    private final static String DEFAULT_STORAGE = "jfrog-storage";


    private static final String USER = "USER";
    private static final String GROUP = "GROUP";
    private static final String PERMISSION = "PERMISSION";

    private static final String REPOSITORY = "REPOSITORY";
    private final static String JFROG_PREFIX = "/artifactory";

    private final static String MIGRATE="migrate_jfrog_data";

    @Resource
    private DictServiceImpl dictService;


    @Async
    @Override
    public void migrate(JfrogMigrateForm form) {
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(form.getUrl() + JFROG_PREFIX).setUsername(form.getUsername()).setPassword(form.getPassword()).build()) {
            Map<String, Long> groupMap = null;
            // 先更新用户组
            if (form.getContents().contains(GROUP)) {
                groupMap = groupMigrate(artifactory);
            }
            // 同步用户及用户组关联关系
            if (form.getContents().contains(USER)) {
                userMigrate(artifactory, groupMap);
            }
            if (form.getContents().contains(REPOSITORY)) {
                // 创建存储空间
                String storageId = StringUtils.isBlank(form.getStorageId()) ? DEFAULT_STORAGE : form.getStorageId();
                form.setStorageId(storageId);
                // 判断存储空间是否存在，不存在新建
                Assert.isTrue(createStorageIfNotExist(form), "failed to create storage");
                // 同步仓库
                repositoryMigrate(storageId, artifactory, form);
                // 同步权限
                if (form.getContents().contains(GROUP) && form.getContents().contains(USER) && form.getContents().contains(REPOSITORY)) {
                    permissionMigrate(artifactory, storageId, groupMap);
                }
            }

        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e.getMessage());
        }

    }

    @Override
    public void changeRepositoryType(MultipartFile file) {
        if (file.isEmpty()) {
            throw new IllegalArgumentException("文件不能为空");
        }
        try {
            BatchChangeListener listener = new BatchChangeListener();
            EasyExcel.read(file.getInputStream(), BatchChangeRepository.class, listener).sheet().doRead();
            List<BatchChangeRepository> repositories = listener.getDoneRepositories();
            for (BatchChangeRepository doneRepository : repositories) {
                String storageId = doneRepository.getStorage();
                Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
                if (storage != null) {
                    RepositoryDto existRepository = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(doneRepository.getRepository());
                    if (existRepository == null) {
                        log.info("无效的仓库名称{}", doneRepository.getRepository());
                        continue;
                    }
                    // 判断仓库类型是否为代理类型
                    if (RepositoryTypeEnum.PROXY.getType().equals(existRepository.getType())) {
                        existRepository.setType(RepositoryTypeEnum.HOSTED.getType());
                        existRepository.setTrashEnabled(true);
                        existRepository.setAllowsDeletion(true);
                        existRepository.setAllowsDeployment(true);
                        existRepository.setAllowsDirectoryBrowsing(true);
                        existRepository.setRemoteRepository(null);
                        try {
                            configurationManagementService.saveRepository(storageId, existRepository);
                            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(existRepository, storageId, doneRepository.getRepository(), SyncRepositoryEnum.ADD_OR_UPDATE);
                            clusterSyncService.syncRepository(syncRepositoryDto);
                        } catch (Exception e) {
                            log.info("仓库{}更新失败",doneRepository.getRepository(),e);
                        }
                    } else {
                        log.info("{}仓库类型不是代理类型，无需转化", doneRepository.getRepository());
                    }
                } else {
                    log.info("{}无效的存储空间", doneRepository.getStorage());
                }
            }
        } catch (IOException e) {
            log.info("仓库类型修改异常{}", e.getMessage(), e);
            throw new RuntimeException(e.getMessage());
        }


    }

    private Map<String, Long> groupMigrate(Artifactory artifactory) {
        try {
            log.info("begin to sync group ");
            Map<String, Long> groupMap = new HashMap<>();
            List<String> groupNames = artifactory.security().groupNames();
            List<UserGroup> groups = new LinkedList<>();
            List<UserGroup> adminGroups = new LinkedList<>();
            for (String groupName : groupNames) {
                Group group = artifactory.security().group(groupName);
                UserGroup userGroup = JfrogMigrateConvert.INSTANCE.jfrogGroupToFolib(group);
                saveIfNotExist(userGroup);
                groups.add(userGroup);
                if (group.isAdminPrivileges()) {
                    adminGroups.add(userGroup);
                }
            }
            groups.forEach(group -> {
                groupMap.put(group.getGroupName(), group.getId());
            });
            // 将是admin的用户组加入admin role里
            if (!adminGroups.isEmpty()) {
                // 获取admin角色的信息
                FolibRole folibRole = folibRoleService.queryById(SystemRole.ADMIN.name());
                RoleDTO roleDetail = folibRoleService.getRoleDetail(SystemRole.ADMIN.name(), folibRole);
                List<AccessUserGroupsDTO> roleGroups = roleDetail.getPrivileges().getGroups();
                for (UserGroup adminGroup : adminGroups) {
                    AccessUserGroupsDTO groupsDTO = new AccessUserGroupsDTO();
                    groupsDTO.setId(String.valueOf(adminGroup.getId()));
                    groupsDTO.setName(adminGroup.getGroupName());
                    roleGroups.add(groupsDTO);
                }
                List<AccessUserGroupsDTO> distinctGroups = new ArrayList<>(roleGroups.stream().collect(Collectors.toMap(AccessUserGroupsDTO::getId, g -> g, (existing, replacement) -> existing)).values());
                roleDetail.getPrivileges().setGroups(distinctGroups);
                folibRoleService.save(roleDetail, UserUtils.getUsername());
                //同步角色信息到其他节点
                privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleDetail.getName());
            }
            log.info("group info sync edn");
            return groupMap;
        } catch (Exception e) {
            log.info("failed to sync group {}", e.getMessage(), e);
            throw new RuntimeException(e.getMessage());
        }
    }

    private void userMigrate(Artifactory artifactory, Map<String, Long> groupMap) {
        Collection<String> userNames = artifactory.security().userNames();
        for (String userName : userNames) {
            User user = artifactory.security().user(userName);
            UserDto newUser = new UserDto();
            newUser.setUsername(userName);
            newUser.setEmail(user.getEmail());
            newUser.setId(userName);
            // 必须为用户赋予一个角色
            if (user.isAdmin()) {
                SecurityRole securityRole = new SecurityRoleEntity(SystemRole.ADMIN.name());
                newUser.setRoles(Collections.singleton(securityRole));
            } else {
                SecurityRole securityRole = new SecurityRoleEntity(SystemRole.GENERAL.name());
                newUser.setRoles(Collections.singleton(securityRole));
            }
            // 判断用户是否存在 存在不操作
            com.veadan.folib.domain.User folibUser = userService.findByUsername(userName);
            if (folibUser != null) {
                continue;
            }
            // 同步用户组信息 如果groupMap为null代表没有同步用户组
            if (groupMap != null) {
                Collection<String> groups = user.getGroups();
                if (groups != null && !groups.isEmpty()) {
                    for (String group : groups) {
                        Long groupId = groupMap.get(group);
                        newUser.getUserGroupIds().add(String.valueOf(groupId));
                    }
                }
            }
            // 设置默认密码等于用户名
            newUser.setPassword("DayeKJjeRQ$4N3z");
            userService.save(new EncodedPasswordUser(newUser, passwordEncoder));
        }
    }

    private void repositoryMigrate(String storageId, Artifactory artifactory, JfrogMigrateForm form) {
        Repositories repositories = artifactory.repositories();
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);

        List<LightweightRepository> repoList = new LinkedList<>();
        repoList.addAll(repositories.list(LOCAL));
        repoList.addAll(repositories.list(REMOTE));
        repoList.addAll(repositories.list(VIRTUAL));
        for (LightweightRepository repository : repoList) {
            String repositoryId = repository.getKey();
            RepositoryDto repositoryDto = JfrogMapping.initRepoByPackageType(repository.getPackageType());
            Repository exist = storage.getRepository(repository.getKey());
            if (exist != null) {
                log.info("the repository {} is exist ,skip", repository.getPackageType());
                continue;
            }
            if (repositoryDto == null) {
                log.error("don't  support the repository {} ", repository.getPackageType());
                continue;
            }
            repositoryDto.setId(repositoryId);
            if("s3".equals(storage.getStorageProvider())){
                String basedir=storage.getBasedir()+"/"+repositoryId;
                repositoryDto.setBasedir(basedir);
            }
            repositoryDto.setStorageProvider(storage.getStorageProvider());
            repositoryDto.setTrashEnabled(true);
            repositoryDto.setAllowsDeletion(true);
            repositoryDto.setAllowsDeployment(true);
            repositoryDto.setAllowsDirectoryBrowsing(true);
            setRepositoryInfo(repository, repositoryDto, artifactory, storageId, form);
            groupRepositoryValid(storageId, repositoryDto);
            RepositoryDto newRepo;
            try {
                configurationManagementService.saveRepository(storageId, repositoryDto);
                newRepo = getMutableConfigurationClone().getStorages().get(storageId).getRepository(repositoryId);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(newRepo));
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
            if (!RepositoryTypeEnum.GROUP.getType().equals(repositoryDto.getType())) {
                //初始化仓库数据
                @SuppressWarnings("all")
                LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                layoutProvider.initData(storageId, repositoryId);
            }
            String resourceId = storageId + "_" + repositoryId;
            com.veadan.folib.entity.Resource resource = resourceService.queryById(resourceId);
            if (Objects.equals(null, resource)) {
                resourceService.insert(com.veadan.folib.entity.Resource.builder()
                        .id(resourceId.toUpperCase())
                        .storageId(storageId)
                        .repositoryId(repositoryId)
                        .build());
            }
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(newRepo, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
            clusterSyncService.syncRepository(syncRepositoryDto);
            //同步资源信息到其他节点
            privilegeEventListenerRegistry.dispatchResourceSyncEvent(storageId + "_" + repositoryId);

        }

    }

    void setRepositoryInfo(LightweightRepository repository, RepositoryDto repositoryDto, Artifactory
            artifactory, String storageId, JfrogMigrateForm form) {
        if (repository.getType() == LOCAL) {
            if ("2".equals(form.getArtifactType())) {
                repositoryDto.setType(RepositoryTypeEnum.PROXY.getType());
                RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
                artifactory.repository(repository.getKey()).get();
                remoteDTO.setUrl(repository.getUrl());
                remoteDTO.setUsername(form.getUsername());
                remoteDTO.setPassword(form.getPassword());
                remoteDTO.setAutoBlocking(true);
                remoteDTO.setDownloadRemoteIndexes(true);
                remoteDTO.setChecksumValidation(true);
                remoteDTO.setAllowsDirectoryBrowsing(true);
                repositoryDto.setRemoteRepository(remoteDTO);
            } else {
                repositoryDto.setType(RepositoryTypeEnum.HOSTED.getType());
            }

        } else if (repository.getType() == REMOTE) {
            // 代理库要获取远程地址
            repositoryDto.setType(RepositoryTypeEnum.PROXY.getType());
            RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
            RemoteRepository remoteRepository = (RemoteRepository) artifactory.repository(repository.getKey()).get();
            remoteDTO.setUrl(repository.getUrl());
            remoteDTO.setUsername(remoteRepository.getUsername());
            remoteDTO.setPassword(remoteRepository.getPassword());
            repositoryDto.setRemoteRepository(remoteDTO);
        } else if (repository.getType() == VIRTUAL) {
            // 组合库要获取子仓库信息
            repositoryDto.setType(RepositoryTypeEnum.GROUP.getType());
            VirtualRepository virtualRepo = (VirtualRepository) artifactory.repository(repository.getKey()).get();
            Collection<String> repositories = virtualRepo.getRepositories();
            Set<String> groupRepository = new HashSet<>();
            for (String repo : repositories) {
                groupRepository.add(storageId + ":" + repo);
            }
            repositoryDto.setGroupRepositories(groupRepository);
        }
    }


    private void permissionMigrate(Artifactory artifactory, String storageId, Map<String, Long> groupMap) {
        List<String> permissionTargetNames = artifactory.security().permissionTargets();
        for (String permissionTargetName : permissionTargetNames) {
            PermissionTarget permission = artifactory.security().permissionTarget(permissionTargetName);
            log.info("jfrog permission is" + JSONObject.toJSONString(permission));
            Principals principals = permission.getPrincipals();
            AccessModelDTO folibPrincipals = new AccessModelDTO();
            List<AccessUserGroupsDTO> folibGroups = new LinkedList<>();
            List<AccessUsersDTO> folibUsers = new LinkedList<>();
            List<AccessResourcesDTO> folibResources = new LinkedList<>();
            // 用户组转换
            for (Principal group : principals.getGroups()) {
                AccessUserGroupsDTO folibGroup = new AccessUserGroupsDTO();
                folibGroup.setName(group.getName());
                folibGroup.setId(String.valueOf(groupMap.get(group.getName())));
                for (Privilege privilege : group.getPrivileges()) {
                    String access = JfrogMapping.accessConvert(privilege);
                    if (StringUtils.isNotBlank(access)) {
                        folibGroup.getAccess().add(access);
                    }
                }
                folibGroups.add(folibGroup);
            }
            // 用户转换 todo anonymous 用户的问题
            for (Principal user : principals.getUsers()) {
                AccessUsersDTO folibUser = new AccessUsersDTO();
                folibUser.setId(user.getName());
                for (Privilege privilege : user.getPrivileges()) {
                    String access = JfrogMapping.accessConvert(privilege);
                    if (StringUtils.isNotBlank(access)) {
                        folibUser.getAccess().add(access);
                    }
                }
                folibUsers.add(folibUser);
            }
            // 资源转换
            Set<String> paths = Arrays.stream(permission.getIncludesPattern().split(",")).collect(Collectors.toSet());
            for (String repository : permission.getRepositories()) {
                if (paths.contains("**")) {
                    AccessResourcesDTO folibResource = new AccessResourcesDTO();
                    folibResource.setStorageId(storageId);
                    folibResource.setRepositoryId(repository);
                    folibResources.add(folibResource);
                } else {
                    for (String path : paths) {
                        AccessResourcesDTO folibResource = new AccessResourcesDTO();
                        folibResource.setStorageId(storageId);
                        folibResource.setRepositoryId(repository);
                        folibResource.setPath(path);
                        folibResources.add(folibResource);
                    }
                }
            }
            // 整合权限对象
            RoleDTO roleDTO = new RoleDTO();
            roleDTO.setName(permission.getName());
            roleDTO.setDescription("Jfrog同步权限:" + permission.getName());
            folibPrincipals.setGroups(folibGroups);
            folibPrincipals.setUsers(folibUsers);
            roleDTO.setPrivileges(folibPrincipals);
            roleDTO.setResources(folibResources);
            folibRoleService.save(roleDTO, UserUtils.getUsername());
            //同步角色信息到其他节点
            privilegeEventListenerRegistry.dispatchRoleSyncEvent(roleDTO.getName());
        }
    }

    public void saveIfNotExist(UserGroup userGroup) {
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = userGroupService.queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups) && userGroups.get(0).getGroupName().equals(groupName)) {
            log.info("UserGroupName {} is already exist update groupName", groupName);
            userGroup.setIsDefault(null);
            userGroup.setDeleted(null);
            userGroup.setId(userGroups.get(0).getId());
        } else {
            userGroup.setId(idGenerateUtils.generateId("userGroupId"));
            userGroupMapper.insert(userGroup);
        }
    }

    public boolean createStorageIfNotExist(JfrogMigrateForm form) {
        // 判断存储空间是否存在
        String storageId = form.getStorageId();
        Storage existStorage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (existStorage == null) {
            StorageDto storage = new StorageDto();
            storage.setId(storageId);
            storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            storage.setBasedir(form.getBasedir());
            if (StringUtils.isBlank(form.getStorageProvider())) {
                storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
            }
            try {
                storageManagementService.createStorage(storage);
                // 向其他集群节点同步storage
                SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storageId, SyncStorageEnum.CREATE);
                clusterSyncService.syncStorage(syncStorageDto);
                //同步资源信息到其他节点
                privilegeEventListenerRegistry.dispatchResourceSyncEvent(storage.getId());
            } catch (Exception e) {
                log.error("create storage failed{}", e.getMessage(), e);
                return false;
            }
        }
        return true;
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
