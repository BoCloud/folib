package com.veadan.folib.users.service.impl;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.converts.ResourceConvert;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.mapper.RoleResourceRefMapper;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
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
    
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public RoleResourceRef queryById(String id){
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
    public boolean deleteById(String id){
        int total = roleResourceRefMapper.deleteById(id);
        return total > 0;
    }

    public boolean deleteByRoleId(String roleId){
        int total = roleResourceRefMapper.delete(RoleResourceRef.builder().roleId(roleId).build());
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
    public void removeByIds(List<String> removeRefIds) {
        roleResourceRefMapper.deleteByRefIds(removeRefIds);
    }

    /**
     * 根据用户id查询关联的角色权限
     * @param userName 用户名
     * @param pageRequest 分页参数
     */
    @Override
    public List<UserRoleDTO> getRolesByUserName(String userName, PageRequest pageRequest) {
        return roleResourceRefMapper.queryRolesByUserName(userName, pageRequest);
    }

    @Override
    public RoleDTO getUserByRoleId(String roleId) {
        return roleResourceRefMapper.getUserByRoleId(roleId);
    }

    @Override
    public List<PermissionsDTO> queryPermissions(String roleId, String username) {
        return roleResourceRefMapper.queryPermissions(roleId, username);
    }

    @Override
    public void savePermissions(RoleDTO roleForm, String roleId, String username) {
        List<AccessResourcesDTO> formResources = roleForm.getResources();
        List<Resource> resources = ResourceConvert.INSTANCE.formToDtoS(formResources);
        List<Resource> allResource = resourceService.findResources(resources);
        List<Resource> addResources = resources.stream().filter(resource -> allResource.stream().noneMatch(resource1 -> resource.getStorageId().equals(resource1.getStorageId()) && resource.getRepositoryId().equals(resource1.getRepositoryId()) && resource.getPath().equals(resource1.getPath()))).collect(Collectors.toList());
        //资源不存在则创建
        resourceService.saveBatch(addResources);
        allResource.addAll(addResources);

        Map<String, Resource> pathMap = allResource.stream().filter(resource -> !Objects.equals(resource.getPath(), null) && !resource.getPath().isEmpty()).collect(Collectors.toMap(Resource::getPath, resource -> resource, (k1, k2)->k1));
        Map<String, Resource> repositoryMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  !Objects.equals(resource.getRepositoryId(), null) && !resource.getRepositoryId().isEmpty()).collect(Collectors.toMap(Resource::getRepositoryId, resource -> resource));
        Map<String, Resource> storageMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  Objects.equals(resource.getRepositoryId(), null) || resource.getRepositoryId().isEmpty()).filter(resource -> !Objects.equals(resource.getStorageId(), null) && !resource.getStorageId().isEmpty()).collect(Collectors.toMap(Resource::getStorageId, resource -> resource));
        resources.forEach(resourceDTO -> {
            Long resourceId = null;
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

        //保存权限
        List<RoleResourceRef> roleResourceRefs = new ArrayList<>();
        AccessModelDTO privileges = roleForm.getPrivileges();
        List<AccessUsersDTO> users = privileges.getUsers();
        List<AccessUserGroupsDTO> groups = privileges.getGroups();
        if (CollectionUtils.isEmpty(users) && CollectionUtils.isEmpty(groups)){
            List<RoleResourceRef> roleResourceRef = resources.stream().map(accessResourcesDTO -> RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).createBy(username).build()).collect(Collectors.toList());
            saveBath(roleResourceRef);
            return;
        }
        //用户权限组装
        if (CollectionUtils.isNotEmpty(users)){
            resources.forEach(accessResourcesDTO -> users.forEach(user -> {
                List<String> access = user.getAccess();
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
            }));
        }
        //用户组权限组装
        if (CollectionUtils.isNotEmpty(groups)){
            resources.forEach(accessResourcesDTO -> groups.forEach(groupsDTO -> {
                List<String> access = groupsDTO.getAccess();
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
            }));
        }

        saveBath(roleResourceRefs);
    }
}