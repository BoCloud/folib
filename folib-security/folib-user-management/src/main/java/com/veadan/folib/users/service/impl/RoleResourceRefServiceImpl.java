package com.veadan.folib.users.service.impl;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.ResourceConvert;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.FolibUser;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.entity.UserGroupRef;
import com.veadan.folib.mapper.RoleResourceRefMapper;
import com.veadan.folib.users.dto.UserPermissionDTO;
import com.veadan.folib.users.service.FolibUserService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import com.veadan.folib.users.service.UserGroupRefService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 权限表;(role_resource_ref)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
@Service
@Transactional
public class RoleResourceRefServiceImpl implements RoleResourceRefService {
    @Autowired
    private RoleResourceRefMapper roleResourceRefMapper;
    @Autowired
    private ResourceService resourceService;
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Autowired
    private FolibUserService folibUserService;
    
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public RoleResourceRef queryById(Long id){
        return roleResourceRefMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param roleResourceRef 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public Page<RoleResourceRef> paginQuery(RoleResourceRef roleResourceRef, PageRequest pageRequest){
        long total = roleResourceRefMapper.count(roleResourceRef);
        return new PageImpl<>(roleResourceRefMapper.queryAllByLimit(roleResourceRef, pageRequest), pageRequest, total);
    }
    
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    public RoleResourceRef insert(RoleResourceRef roleResourceRef){
        roleResourceRefMapper.insert(roleResourceRef);
        return roleResourceRef;
    }
    
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    public RoleResourceRef update(RoleResourceRef roleResourceRef){
        roleResourceRefMapper.update(roleResourceRef);
        return queryById(roleResourceRef.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(Long id){
        int total = roleResourceRefMapper.deleteById(id);
        return total > 0;
    }

    public boolean deleteByRoleId(String roleId){
        int total = roleResourceRefMapper.deleteByRoleId(roleId);
        return total > 0;
    }

    @Override
    public List<RoleResourceRef> queryRoleByUserId(String uuid, List<String> roles) {
        return roleResourceRefMapper.queryRoleByUserId(uuid, roles);
    }

    @Override
    public int saveBath(List<RoleResourceRef> roleResourceRefs) {
        return roleResourceRefMapper.insertBatch(roleResourceRefs);
    }

    @Override
    public List<RoleResourceRef> queryRefs(RoleResourceRef roleResourceRef) {
        return roleResourceRefMapper.select(roleResourceRef);
    }

    @Override
    public List<RoleResourceRef> queryRefsByRoleIds(List<String> roleIds) {
        return this.roleResourceRefMapper.queryAllByRoleId(roleIds);
    }

    @Override
    public void removeByIds(List<Long> removeRefIds) {
        roleResourceRefMapper.deleteByRefIds(removeRefIds);
    }

    /**
     * 根据用户id查询关联的角色权限
     * @param userName 用户名
     */
    @Override
    public List<UserRoleDTO> getRolesByUserName(String userName) {
        List<String> roleIdsList = getRoleListByUserName(userName);
        if (CollectionUtils.isEmpty(roleIdsList)) {
            return null;
        }
        return roleResourceRefMapper.queryPrivileges(roleIdsList);
    }

    /**
     * 根据用户名查询角色id
     * @param userName
     * @return 角色关联列表
     */
    private List<String> getRoleListByUserName(String userName) {
        if (StringUtils.isEmpty(userName)) {
            return null;
        }
        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByUserId(userName);
        Set<Long> groupIds = null;
        if (CollectionUtils.isNotEmpty(userGroupRefs)) {
            groupIds = userGroupRefs.stream().map(UserGroupRef::getUserGroupId).collect(Collectors.toSet());
        }
        Example example = Example.builder(RoleResourceRef.class).build();
        example.createCriteria().andEqualTo("refType", GlobalConstants.ROLE_TYPE_USER).andEqualTo("entityId", userName);
        if (CollectionUtils.isNotEmpty(groupIds)) {
            example.or().andEqualTo("refType", GlobalConstants.ROLE_TYPE_USER_GROUP).andEqualTo("entityId", groupIds);
        }
        List<RoleResourceRef> roleResourceRefs = roleResourceRefMapper.selectByExample(example);
        return roleResourceRefs.stream().map(RoleResourceRef::getRoleId).distinct().collect(Collectors.toList());
    }

    @Override
    public RoleDTO getUserByRoleId(String roleId) {
        return roleResourceRefMapper.getUserByRoleId(roleId);
    }

    @Override
    public List<PermissionsDTO> queryPermissions(String roleId, String username, String storageId, String repositoryId) {
        return roleResourceRefMapper.queryPermissions(roleId, username, storageId, repositoryId, null, true);
    }

    @Override
    public List<PermissionsDTO> queryPermissions(String roleId, String username, String storageId, String repositoryId, boolean resourceEmpty) {
        return roleResourceRefMapper.queryPermissions(roleId, username, storageId, repositoryId, null, resourceEmpty);
    }

    @Override
    public List<PermissionsDTO> queryPermissionsByResourceIds(List<String> resourceIds) {
        return roleResourceRefMapper.queryPermissions(null, null, null, null, resourceIds, false);
    }

    @Override
    public void savePermissions(RoleDTO roleForm, String roleId, String username) {
        List<AccessResourcesDTO> formResources = roleForm.getResources();
        List<Resource> resources = ResourceConvert.INSTANCE.formToDtoS(formResources);
        if (CollectionUtils.isNotEmpty(resources)) {
            List<Resource> allResource = resourceService.findResources(resources);
            List<Resource> addResources = resources.stream().filter(resource -> allResource.stream().noneMatch(resource1 -> resource.getStorageId().equals(resource1.getStorageId()) && Objects.equals(resource.getRepositoryId(), resource1.getRepositoryId()) && Objects.equals(resource.getPath(), resource1.getPath()))).collect(Collectors.toList());
            //资源不存在则创建
            if (CollectionUtils.isNotEmpty(addResources)) {
                resourceService.saveBatch(addResources);
                allResource.addAll(addResources);
            }

            Map<String, Resource> pathMap = allResource.stream().filter(resource -> !Objects.equals(resource.getPath(), null) && !resource.getPath().isEmpty()).collect(Collectors.toMap(Resource::getPath, resource -> resource, (k1, k2)->k1));
            Map<String, Resource> repositoryMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  !Objects.equals(resource.getRepositoryId(), null) && !resource.getRepositoryId().isEmpty()).collect(Collectors.toMap(Resource::getRepositoryId, resource -> resource));
            Map<String, Resource> storageMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  Objects.equals(resource.getRepositoryId(), null) || resource.getRepositoryId().isEmpty()).filter(resource -> !Objects.equals(resource.getStorageId(), null) && !resource.getStorageId().isEmpty()).collect(Collectors.toMap(Resource::getStorageId, resource -> resource));

            resources.forEach(resourceDTO -> {
                String resourceId = null;
                // 根据 path 查找
                if (resourceDTO.getPath() != null && !resourceDTO.getPath().isEmpty()) {
                    resourceId = pathMap.get(resourceDTO.getPath()).getId();
                }
                // 根据 repositoryId 查找
                if (resourceId == null && resourceDTO.getRepositoryId() != null) {
                    resourceId = repositoryMap.get(resourceDTO.getRepositoryId()).getId();
                }
                // 根据 storageId 查找
                if (resourceId == null && resourceDTO.getStorageId() != null) {
                    resourceId = storageMap.get(resourceDTO.getStorageId()).getId();
                }
                // 将 resourceId 赋值回 resourceDTO
                resourceDTO.setId(resourceId);
            });
        }


        //保存权限
        List<RoleResourceRef> roleResourceRefs = new ArrayList<>();
        AccessModelDTO privileges = roleForm.getPrivileges();
        List<AccessUsersDTO> users = privileges.getUsers();
        List<AccessUserGroupsDTO> groups = privileges.getGroups();
        if (CollectionUtils.isEmpty(users) && CollectionUtils.isEmpty(groups)){
            if (CollectionUtils.isNotEmpty(resources)) {
                List<RoleResourceRef> roleResourceRef = resources.stream().map(accessResourcesDTO -> RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).createBy(username).build()).collect(Collectors.toList());
                saveBath(roleResourceRef);
            }
            return;
        }
        //用户权限组装
        if (CollectionUtils.isNotEmpty(users)){
             users.forEach(user -> {
                 if (CollectionUtils.isNotEmpty(resources)) {
                     resources.forEach(accessResourcesDTO -> {
                         List<String> access = user.getAccess();
                         if (CollectionUtils.isNotEmpty(access)) {
                             access.forEach(pri -> {
                                 if(StringUtils.isEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                             .storagePrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                                 }

                                 if(StringUtils.isNotEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                             .repositoryPrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build());
                                 }

                                 if (StringUtils.isNotEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER).pathPrivilege(pri)
                                             .resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build());
                                 }
                             });
                         }else {
                             roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                     .resourceId(accessResourcesDTO.getId()).createBy(username).build());
                         }
                     });
                 }else {
                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build());
                 }
             });

        }
        //用户组权限组装
        if (CollectionUtils.isNotEmpty(groups)){
             groups.forEach(groupsDTO -> {
                 if (CollectionUtils.isNotEmpty(resources)) {
                     resources.forEach(accessResourcesDTO ->{
                         List<String> access = groupsDTO.getAccess();
                         if (CollectionUtils.isNotEmpty(access)) {
                             access.forEach(pri -> {
                                 if(StringUtils.isEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                             .storagePrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                                 }

                                 if(StringUtils.isNotEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                             .repositoryPrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build());
                                 }
                                 if(StringUtils.isNotEmpty(accessResourcesDTO.getPath())) {
                                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).pathPrivilege(pri)
                                             .resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build());
                                 }
                             });
                         }else {
                             roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                     .resourceId(accessResourcesDTO.getId()).createBy(username).build());
                         }
                     });
                 }else {
                     roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).createBy(username).build());
                 }
             });
        }

        saveBath(roleResourceRefs);
    }

    @Override
    public List<RoleResourceRef> queryApiAuthorities(List<String> roleIds) {
        return roleResourceRefMapper.queryApiAuthorities(roleIds);
    }

    @Override
    public void batchUpdate(List<RoleResourceRef> userRoles) {
        List<String> roleIds = userRoles.stream().map(RoleResourceRef::getRoleId).collect(Collectors.toList());
        List<RoleResourceRef> queryUserRoleRefs = roleResourceRefMapper.queryByRoleIds(roleIds);
        if (!queryUserRoleRefs.isEmpty()) {
            userRoles = userRoles.stream().filter(userRoleRef -> queryUserRoleRefs.stream().noneMatch(queryUserGroupRef ->
                    queryUserGroupRef.getRoleId().equals(userRoleRef.getRoleId())
                            && Objects.equals(queryUserGroupRef.getEntityId(), userRoleRef.getEntityId())
                            && Objects.equals(queryUserGroupRef.getRefType(), userRoleRef.getRefType())
                            && Objects.equals(queryUserGroupRef.getResourceId(), userRoleRef.getResourceId())
                            && Objects.equals(queryUserGroupRef.getResourceType(), userRoleRef.getResourceType())
                            && Objects.equals(queryUserGroupRef.getPathPrivilege(), userRoleRef.getPathPrivilege())
                            && Objects.equals(queryUserGroupRef.getRepositoryPrivilege(), userRoleRef.getRepositoryPrivilege())
                            && Objects.equals(queryUserGroupRef.getStoragePrivilege(), userRoleRef.getStoragePrivilege()))).collect(Collectors.toList());
        }
        if (CollectionUtils.isNotEmpty(userRoles)) {
            roleResourceRefMapper.insertBatch(userRoles);
        }
    }

    @Override
    public List<RoleResourceRef> queryByRoleIds(List<String> roleIds) {
        return roleResourceRefMapper.queryByRoleIds(roleIds);
    }

    @Override
    public List<FolibUser> queryUserByRoleIds(String roleId) {
        List<RoleResourceRef> roleResourceRefs = roleResourceRefMapper.queryByRoleIds(Collections.singletonList(roleId));
        List<String> userIds = roleResourceRefs.stream().filter(roleResourceRef -> GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());

        List<Long> groupIds = roleResourceRefs.stream().filter(roleResourceRef -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).map(Long::valueOf).collect(Collectors.toList());
        if(CollectionUtils.isNotEmpty(groupIds)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (CollectionUtils.isNotEmpty(userGroupRefs)){
                userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList()));
            }
        }

        return folibUserService.queryByIds(userIds);
    }

    @Override
    public void deleteAllByRoleId(String roleId) {
        Example example = new Example(RoleResourceRef.class);
        example.createCriteria().andEqualTo("roleId", roleId).andIsNull("entityId");
        roleResourceRefMapper.deleteByExample(example);
    }

    @Override
    public void updateStorageUser(UserPermissionDTO userPermission) {
        if (CollectionUtils.isEmpty(userPermission.getRoleIds())) {
            return;
        }
        List<String> privileges = new ArrayList<>(userPermission.getPrivileges());
        List<String> roleIds = new ArrayList<>(userPermission.getRoleIds());
        String userId = userPermission.getUserId();
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(roleIds);
        if (CollectionUtils.isEmpty(roleResourceRefs)) {
            return;
        }
        //清理用户已关联的角色
        List<Long> removeRefIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) &&
                Objects.equals(userId, ref.getEntityId()) && !privileges.contains(ref.getPathPrivilege())).map(RoleResourceRef::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(removeRefIds)) {
            deleteByIds(removeRefIds);
        }
        List<String> refRoleIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) &&
                Objects.equals(userId, ref.getEntityId()) && privileges.contains(ref.getPathPrivilege())).map(RoleResourceRef::getRoleId).collect(Collectors.toList());
        roleIds.removeAll(refRoleIds);

        //保存关联权限
        if(CollectionUtils.isNotEmpty(roleIds)) {
            List<RoleResourceRef> updateRefs = new ArrayList<>();
            roleIds.forEach(roleId -> {
                if (CollectionUtils.isNotEmpty(privileges)) {
                    privileges.forEach(privilege -> {
                        updateRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).pathPrivilege(privilege).resourceId(roleId.replaceFirst("STORAGE_USER_", "")).build());
                    });
                }else {
                    updateRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER)
                            .resourceId(roleId.replaceFirst("STORAGE_USER_", "")).build());
                }
            });
            if (CollectionUtils.isNotEmpty(updateRefs)) {
                saveBath(updateRefs.stream().distinct().collect(Collectors.toList()));
            }
        }

    }

    @Override
    public void deleteByResourceIds(List<String> resourceIds) {
        Example example = Example.builder(RoleResourceRef.class).build();
        example.createCriteria().andIn("resourceId", resourceIds);
        roleResourceRefMapper.deleteByExample(example);
    }

    @Override
    public List<RoleResourceRef> queryByResourceIds(List<String> resourceIds) {
        Example example = Example.builder(RoleResourceRef.class).build();
        example.createCriteria().andIn("resourceId", resourceIds);
        return roleResourceRefMapper.selectByExample(example);
    }

    @Override
    public void deleteByentityId(String entityId, String refType) {
        Example example = new Example(RoleResourceRef.class);
        example.createCriteria().andEqualTo("entityId", entityId).andEqualTo("refType", refType);
        roleResourceRefMapper.deleteByExample(example);
    }

    @Override
    public void deleteByIds(List<Long> removeIds) {
        Example example = new Example(RoleResourceRef.class);
        example.createCriteria().andIn("id", removeIds);
        roleResourceRefMapper.deleteByExample(example);
    }

    @Override
    public List<RoleResourceRef> queryPermissionsByRoleIds(List<String> roleIds) {
        Example example = Example.builder(RoleResourceRef.class).build();
        example.createCriteria().andIn("roleId", roleIds);
        return roleResourceRefMapper.selectByExample(example);
    }

    @Override
    public List<RoleResourceRef> queryByUserIds(List<String> userIds) {
        Example example = Example.builder(RoleResourceRef.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andIn("entityId", userIds).andEqualTo("refType", GlobalConstants.ROLE_TYPE_USER);
        return roleResourceRefMapper.selectByExample(example);

    }

    @Override
    public void updateUserPermission(Set<UserPermissionDTO> userPermissions) {
        if (CollectionUtils.isEmpty(userPermissions)) {
            return;
        }
        Set<String> roleIds = userPermissions.stream().map(UserPermissionDTO::getRoleIds).flatMap(Collection::stream).collect(Collectors.toSet());
        Set<String> userIds = userPermissions.stream().map(UserPermissionDTO::getUserId).collect(Collectors.toSet());
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(new ArrayList<>(roleIds));
        if (CollectionUtils.isEmpty(roleResourceRefs)) {
            return;
        }
        //清理用户已关联的角色
        List<Long> removeRefIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) && userIds.contains(ref.getEntityId())).map(RoleResourceRef::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(removeRefIds)) {
            deleteByIds(removeRefIds);
        }

        //保存关联权限
        Map<String, List<RoleResourceRef>> roleMap = roleResourceRefs.stream().filter(ref -> StringUtils.isNotEmpty(ref.getResourceType())).map(ref -> RoleResourceRef.builder().roleId(ref.getRoleId()).resourceId(ref.getResourceId()).resourceType(ref.getResourceType())
                .storagePrivilege(ref.getStoragePrivilege()).repositoryPrivilege(ref.getRepositoryPrivilege()).pathPrivilege(ref.getPathPrivilege()).build()).distinct().collect(Collectors.groupingBy(RoleResourceRef::getRoleId));
        List<RoleResourceRef> updateRefs = new ArrayList<>();
        userPermissions.forEach(userPermission -> {
            String userId = userPermission.getUserId();
            Collection<String> userRoleIds = userPermission.getRoleIds();
            Collection<String> privileges = userPermission.getPrivileges();
            userRoleIds.forEach(roleId -> {
                List<RoleResourceRef> resourceRefs = roleMap.get(roleId);
                if (CollectionUtils.isNotEmpty(resourceRefs)) {
                    resourceRefs.forEach(resourceRef -> privileges.forEach(privilege -> {
                        RoleResourceRef.RoleResourceRefBuilder builder = RoleResourceRef.builder();
                        switch(resourceRef.getResourceType()){
                            case GlobalConstants.RESOURCE_TYPE_STORAGE:
                                builder.storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                                break;
                            case GlobalConstants.RESOURCE_TYPE_REPOSITORY:
                                builder.repositoryPrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                                break;
                            case GlobalConstants.RESOURCE_TYPE_PATH:
                                builder.pathPrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                                break;
                        }
                        builder.entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).roleId(roleId).resourceId(resourceRef.getResourceId());
                        updateRefs.add(builder.build());
                    }));
                }
            });
        });
        if (CollectionUtils.isNotEmpty(updateRefs)) {
            saveBath(updateRefs.stream().distinct().collect(Collectors.toList()));
        }
    }
}