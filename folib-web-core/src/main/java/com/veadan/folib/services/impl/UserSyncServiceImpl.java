package com.veadan.folib.services.impl;


import com.veadan.folib.entity.*;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.*;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService.RelationalDatabase;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Slf4j
@Component
@RelationalDatabase
@Transactional(rollbackFor = Exception.class)
public class UserSyncServiceImpl implements UserSyncService
{

    @Inject
    protected FolibUserService folibUserService;
    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private ResourceService resourceService;
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private StorageManagementService storageManagementService;
    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Autowired
    private RepositoryManagementService repositoryManagementService;

    @Override
    @Transactional
    public void syncUserAuth(UserAuthDTO date) {

        //更新节点用户信息
        List<FolibUser> users = date.getUsers();
        if (CollectionUtils.isNotEmpty(users)) {
            folibUserService.saveOrUpdate(users);
        }
        //更新用户组信息
        List<UserGroup> groups = date.getGroups();
        List<UserGroupRef> userGroups = date.getUserGroups();
        if (CollectionUtils.isNotEmpty(groups) || CollectionUtils.isNotEmpty(userGroups)) {
            List<String> groupNames = groups.stream().map(UserGroup::getGroupName).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(userGroups)) {
                groupNames.addAll(userGroups.stream().map(UserGroupRef::getUserGroupName).collect(Collectors.toList()));
            }
            List<UserGroup> userGroupList = userGroupService.queryByGroupNames(groupNames);
            Map<String, Long> userGroupMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(userGroupList)) {
                userGroupMap = userGroupList.stream().collect(Collectors.toMap(UserGroup::getGroupName, UserGroup::getId, (existing, replacement) -> existing));
            }

            if (CollectionUtils.isNotEmpty(groups)) {
                Map<String, Long> finalUserGroupMap = userGroupMap;
                List<UserGroup> addGroups = new ArrayList<>();
                groups.forEach(userGroup -> {
                    Long groupId = finalUserGroupMap.get(userGroup.getGroupName());
                    if (groupId == null) {
                        addGroups.add(userGroup);
                    }
                });
                if (CollectionUtils.isNotEmpty(addGroups)) {
                    userGroupService.saveOrUpdateBatch(addGroups);
                }
            }
            //更新用户组关联信息
            if (CollectionUtils.isNotEmpty(userGroups)) {
                Map<String, Long> finalUserGroupMap1 = userGroupMap;
                userGroups.forEach(userGroupRef -> {
                        Long groupId = finalUserGroupMap1.get(userGroupRef.getUserGroupName());
                        if (groupId != null) {
                            userGroupRef.setUserGroupId(groupId);
                        }
                    });
                userGroupRefService.batchUpdate(userGroups);
            }
        }


        //更新角色信息
        List<FolibRole> roles = date.getRoles();
        if (CollectionUtils.isNotEmpty(roles)) {
            folibRoleService.saveOrUpdateBatch(roles);
        }
        //更新资源信息
        List<Resource> resources = date.getResources();
        if (CollectionUtils.isNotEmpty(resources)) {
            resourceService.saveOrUpdateBatch(resources);
        }
        //更新角色关联信息
        List<RoleResourceRef> userRoles = date.getUserRoles();
        if (CollectionUtils.isNotEmpty(userRoles)) {
            roleResourceRefService.batchUpdate(userRoles);
        }
        //更新存储信息
        List<StorageDto> storages = date.getStorages();
        if (CollectionUtils.isNotEmpty(storages)) {
            storages.forEach(storage -> {
                Storage storageInfo = configurationManagementService.getMutableConfigurationClone().getStorage(storage.getId());
                if (storageInfo == null) {
                    try {
                        configurationManagementService.createStorage(storage);
                    } catch (IOException e) {
                        log.error("创建存储失败", e);
                    }
                }else {
                    try {
                        configurationManagementService.updateStorage(storage);
                    } catch (IOException e) {
                        log.error("更新存储失败", e);
                    }
                }
            });
        }
        List<RepositoryDto> repositorys = date.getRepositorys();
        if (CollectionUtils.isNotEmpty(repositorys)) {
            repositorys.forEach(repository -> {
                String storageId = repository.getStorage().getId();
                String repositoryId = repository.getId();
                StorageDto storageDto = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                if (storageDto == null) {
                    try {
                        configurationManagementService.createStorage(storageDto);
                    } catch (IOException e) {
                        log.error("创建仓库关联的存储失败", e);
                    }
                }
                Repository existRepository = storageDto.getRepository(repositoryId);
                boolean result = Objects.nonNull(existRepository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
                if (!result) {
                    try {
                        //判断重复
                        configurationManagementService.addOrUpdateRepository(storageId, repository);
                    } catch (Exception e) {
                        log.error("新增、更新仓库失败", e);
                    }
                }
            });
        }
        //清理已删除的用户权限信息
        removeUserAuth(date);
    }

    /**
     *
     * 方法描述:  清理已删除的用户资源
     *
     * @param: 已删除的用户资源
     */
    private void removeUserAuth(UserAuthDTO date) {
        //删除用户信息
        List<String> removeUserIds = date.getRemoveUserIds();
        if (CollectionUtils.isNotEmpty(removeUserIds)) {
            String userId = removeUserIds.get(0);
            folibUserService.deleteByUserName(userId);
        }
        //删除用户组信息
        List<Long> removeGroupIds = date.getRemoveGroupIds();
        if (CollectionUtils.isNotEmpty(removeGroupIds)) {
            Long groupId = removeGroupIds.get(0);
            userGroupService.deleteById(groupId);
        }
        //删除角色信息
        List<String> removeRoleIds = date.getRemoveRoleIds();
        if (CollectionUtils.isNotEmpty(removeRoleIds)) {
            String roleId = removeRoleIds.get(0);
            folibRoleService.deleteById(roleId);
        }
        //删除资源信息
        List<String> removeResourceIds = date.getRemoveResourceIds();
        if (CollectionUtils.isNotEmpty(removeResourceIds)) {
            resourceService.deleteByIds(removeResourceIds);
            roleResourceRefService.deleteByResourceIds(removeResourceIds);

           /* String resourceId = removeResourceIds.get(0);
            Resource resource = resourceService.queryById(resourceId);
            if (resource != null) {
                String repositoryId = resource.getRepositoryId();
                String storageId = resource.getStorageId();
                if (StringUtils.isNotEmpty(repositoryId)) {
                    resourceService.deleteById(resourceId);

                    //删除仓库并同步到集群
                    try {
                        List<Repository> repositoryList = configurationManagementService.getConfiguration().getGroupRepositoriesContaining(storageId, repositoryId);
                        if (CollectionUtils.isNotEmpty(repositoryList)) {
                            log.error(String.format("删除仓库失败，存在关联组合库，请先从组合库[%s]中移除 !", repositoryList.stream().map(item -> String.format("%s:%s", item.getStorage().getId(), item.getId())).collect(Collectors.joining(","))));
                        }
                        Repository repositoryInfo = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId);
                        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repositoryInfo);
                        RepositoryDto repositoryDto = configurationManagementService.getMutableConfigurationClone().getStorage(storageId)
                                .getRepository(repositoryId);
                        if (Files.exists(repositoryPath)) {
                            repositoryManagementService.removeRepository(storageId, repositoryId);
                            repositoryEventListenerRegistry.dispatchRepoDelteToCronJobDeleteEvent(storageId, repositoryId);
                        }

                        configurationManagementService.removeRepository(storageId, repositoryId);
                        SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repositoryDto, storageId, repositoryId, SyncRepositoryEnum.DELETE, true);
                        clusterSyncService.syncRepository(syncRepositoryDto);
                    } catch (IOException e) {
                        log.error("删除仓库[{}]失败！", repositoryId, e);
                    }

                }else {
                    List<Resource> resourcesList = resourceService.queryByStorageId(storageId);
                    List<String> resourceIds = resourcesList.stream().map(Resource::getId).collect(Collectors.toList());
                    resourceService.deleteByIds(resourceIds);

                    //删除存储空间并同步到集群
                    try {
                        storageManagementService.removeStorage(resourceId);
                        repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(resourceId, "");
                        configurationManagementService.removeStorage(resourceId);

                        log.info("Removed storage {}.", resourceId);
                        StorageDto storageDto = configurationManagementService.getMutableConfigurationClone().getStorage(resourceId);
                        SyncStorageDto syncStorageDto = new SyncStorageDto(storageDto, SyncStorageEnum.DELETE, resourceId, true);
                        clusterSyncService.syncStorage(syncStorageDto);
                    } catch (IOException e) {
                        log.error("删除存储空间[{}]失败！", storageId, e);
                    }
                }

            }*/
        }
    }

}
