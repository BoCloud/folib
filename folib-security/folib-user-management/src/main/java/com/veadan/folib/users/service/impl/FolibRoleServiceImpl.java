package com.veadan.folib.users.service.impl;

import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.dto.AccessModelDTO;
import com.veadan.folib.dto.RepositoryAccessModelDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.AccessModelDto;
import com.veadan.folib.users.dto.RepositoryPrivilegesDto;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.ResourceService;
import com.veadan.folib.users.service.RoleResourceRefService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.mapper.FolibRoleMapper;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 角色信息;(folib_role)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Service
@Transactional
public class FolibRoleServiceImpl implements FolibRoleService {
    @Autowired
    private FolibRoleMapper folibRoleMapper;
    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;
    @Inject
    private ResourceService resourceService;
    @Inject
    private RoleResourceRefService roleResourceRefService;

    @Override
    public FolibRole queryByRoleId(List<String> roleIds) {
        return null;
    }

    public void syncYamlAuthorizationConfig() {
        try {
            AuthorizationConfigDto read = authorizationConfigFileManager.read();
            Set<RoleDto> roles = read.getRoles();
            if(CollectionUtils.isNotEmpty(roles)) {
                List<FolibRole> folibRoles = new ArrayList<>(roles.size());
                List<RoleResourceRef> pathPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> storagePrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> repositoryPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> apiUserPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> allRefs = new ArrayList<>();
                List<Resource> resources = new ArrayList<>();

                roles.forEach(roleDto -> {
                    folibRoles.add(FolibRole.builder().id(roleDto.getName()).description(roleDto.getDescription())
                            .enName(roleDto.getName()).deleted(GlobalConstants.NOT_DELETED).isDefault(GlobalConstants.NOT_DEFALUT).cnName(roleDto.getDescription()).build());
                    if(!"admin".equalsIgnoreCase(roleDto.getName())){
                        AccessModelDto accessModel = roleDto.getAccessModel();
                        if(accessModel != null) {
                            accessModel.getApiAuthorities().forEach(privileges -> {
                                //resources.add(Resource.builder().apiAuthoritie(privileges.getAuthority()).build());
                                apiUserPrivilegeRoles.add(RoleResourceRef.builder().roleId(roleDto.getName()).resourceType(GlobalConstants.RESOURCE_TYPE_API).apiAuthoritie(privileges.getAuthority()).build());
                            });
                            accessModel.getStorageAuthorities().forEach(storagePrivilegesDto -> {
                                resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).build());
                                Set<Privileges> storagePrivileges = storagePrivilegesDto.getStoragePrivileges();
                                if(CollectionUtils.isNotEmpty(storagePrivileges)){
                                    List<RoleResourceRef> storagePrivilegeRef = storagePrivileges.stream().map(privilege ->
                                            RoleResourceRef.builder().roleId(roleDto.getName()).storageId(storagePrivilegesDto.getStorageId()).storageProvilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
                                    storagePrivilegeRoles.addAll(storagePrivilegeRef);
                                }

                                Set<RepositoryPrivilegesDto> repositorytories = storagePrivilegesDto.getRepositoryPrivileges();
                                if(CollectionUtils.isNotEmpty(repositorytories)){
                                    repositorytories.forEach(repositoryPrivilegesDto -> {
                                        resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).build());
                                        Set<Privileges> repositoryPrivileges = repositoryPrivilegesDto.getRepositoryPrivileges();
                                        List<RoleResourceRef> repositoryRef = repositoryPrivileges.stream().map(privilege ->
                                                RoleResourceRef.builder().roleId(roleDto.getName()).storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).repositoryPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build()).collect(Collectors.toList());
                                        repositoryPrivilegeRoles.addAll(repositoryRef);

                                        repositoryPrivilegesDto.getPathPrivileges().forEach(pathPrivilegesDto -> {
                                            resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).path(pathPrivilegesDto.getPath()).build());
                                            Set<Privileges> privileges = pathPrivilegesDto.getPrivileges();
                                            List<RoleResourceRef> pathRef = privileges.stream().map(privilege ->
                                                    RoleResourceRef.builder().roleId(roleDto.getName()).path(pathPrivilegesDto.getPath()).pathPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build()).collect(Collectors.toList());
                                            pathPrivilegeRoles.addAll(pathRef);
                                        });
                                    });
                                }
                            });
                        }
                    }
                });
                //admin权限补全
                EnumSet<Privileges> allPrivileges = Privileges.all();
                allPrivileges.forEach(privileges -> {
                    resources.add(Resource.builder().apiAuthoritie(String.valueOf(privileges)).build());
                    /*storagePrivilegeRoles.add(RoleResourceRef.builder().roleId("ADMIN").resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).storageProvilege(privileges.getAuthority()).build());
                    repositoryPrivilegeRoles.add(RoleResourceRef.builder().roleId("ADMIN").resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).repositoryPrivilege(privileges.getAuthority()).build());
                    pathPrivilegeRoles.add(RoleResourceRef.builder().roleId("ADMIN").resourceType(GlobalConstants.RESOURCE_TYPE_PATH).pathPrivilege(privileges.getAuthority()).build());*/
                });
                //角色入库
                if(CollectionUtils.isNotEmpty(folibRoles)){
                    folibRoleMapper.insertOrUpdateBatch(folibRoles);
                }
                //资源入库
                List<Resource> resourceList = filterResource(resources);
                if(CollectionUtils.isNotEmpty(resourceList)) {
                    resourceService.saveBatch(resourceList.stream().distinct().collect(Collectors.toList()));

                    Map<String, Resource> pathMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getPath())).collect(Collectors.toMap(Resource::getPath, resource -> resource, (k1, k2)->k1));
                    Map<String, Resource> storageMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getStorageId())).collect(Collectors.toMap(Resource::getStorageId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> repositoryMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getRepositoryId())).collect(Collectors.toMap(Resource::getRepositoryId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> apiMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getApiAuthoritie())).collect(Collectors.toMap(Resource::getApiAuthoritie, resource -> resource, (k1,k2)->k1));
                    //权限入库
                    List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(folibRoles.stream().map(FolibRole::getId).collect(Collectors.toList()));
                    Map<String, List<RoleResourceRef>> userRoles = roleResourceRefs.stream().collect(Collectors.groupingBy(RoleResourceRef::getRoleId));

                    storagePrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                        roleResourceRef.setStorageProvilege(roleResourceRef.getStorageProvilege());
                        if (storageMap.containsKey(roleResourceRef.getStorageId())) {
                            roleResourceRef.setResourceId(storageMap.get(roleResourceRef.getStorageId()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                                roleResourceRef.setStorageProvilege(ref.getStorageProvilege());
                                if (storageMap.containsKey(roleResourceRef.getStorageId())) {
                                    roleResourceRef.setResourceId(storageMap.get(roleResourceRef.getStorageId()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    repositoryPrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setRepositoryPrivilege(roleResourceRef.getRepositoryPrivilege());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                        if (repositoryMap.containsKey(roleResourceRef.getRepositoryId())) {
                            roleResourceRef.setResourceId(repositoryMap.get(roleResourceRef.getRepositoryId()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setRepositoryPrivilege(ref.getRepositoryPrivilege());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                                if (repositoryMap.containsKey(roleResourceRef.getRepositoryId())) {
                                    roleResourceRef.setResourceId(repositoryMap.get(roleResourceRef.getRepositoryId()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    pathPrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setPathPrivilege(roleResourceRef.getPathPrivilege());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                        if (pathMap.containsKey(roleResourceRef.getPath())) {
                            roleResourceRef.setResourceId(pathMap.get(roleResourceRef.getPath()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setPathPrivilege(ref.getPathPrivilege());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                                if (pathMap.containsKey(roleResourceRef.getPath())) {
                                    roleResourceRef.setResourceId(pathMap.get(roleResourceRef.getPath()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    allRefs.addAll(apiUserPrivilegeRoles);

                    if (CollectionUtils.isNotEmpty(allRefs)) {
                        List<Resource> allResources = resourceService.findAll();
                        Map<String, Resource> resourceMap = allResources.stream().flatMap(resource -> Stream.of(
                                new AbstractMap.SimpleEntry<>(resource.getApiAuthoritie(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getStorageId(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getRepositoryId(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getPath(), resource)
                        )).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> v1));
                        List<RoleResourceRef> collect = allRefs.parallelStream().peek(roleResourceRef -> {
                            String apiAuthoritie = roleResourceRef.getApiAuthoritie();
                            String path = roleResourceRef.getPath();
                            String repositoryId = roleResourceRef.getRepositoryId();
                            String storageId = roleResourceRef.getStorageId();
                            if (StringUtils.isNotEmpty(apiAuthoritie)) {
                                roleResourceRef.setResourceId(resourceMap.get(apiAuthoritie).getId());
                            } else if (StringUtils.isNotEmpty(path)) {
                                roleResourceRef.setResourceId(resourceMap.get(path).getId());
                            } else if (StringUtils.isNotEmpty(repositoryId)) {
                                roleResourceRef.setResourceId(resourceMap.get(repositoryId).getId());
                            } else if (StringUtils.isNotEmpty(storageId)) {
                                roleResourceRef.setResourceId(resourceMap.get(storageId).getId());
                            }
                        }).collect(Collectors.toList());

                        roleResourceRefService.saveBath(collect);
                    }
                    //清理未关联资源的用户权限
                    List<String> removeRefIds = roleResourceRefs.stream().filter(ref -> StringUtils.isNotEmpty(ref.getResourceType())).map(RoleResourceRef::getId).collect(Collectors.toList());
                    if(CollectionUtils.isNotEmpty(removeRefIds)) {
                        roleResourceRefService.removeByIds(removeRefIds);
                    }
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("获取yaml权限配置异常");
        }
    }

    private List<Resource> filterResource(List<Resource> resourceList) {
        List<Resource> resources = resourceService.findAll();
        List<String> storagetIds = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getStorageId())).map(Resource::getStorageId).collect(Collectors.toList());
        List<String> repositoryIds = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getRepositoryId())).map(Resource::getRepositoryId).collect(Collectors.toList());
        List<String> paths = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getPath())).map(Resource::getPath).collect(Collectors.toList());
        List<String> apis = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getApiAuthoritie())).map(Resource::getApiAuthoritie).collect(Collectors.toList());

        return resourceList.stream().filter(res -> {
            String repositoryId = res.getRepositoryId();
            String storageId1 = res.getStorageId();
            String apiAuthoritie = res.getApiAuthoritie();
            String path = res.getPath();

            return !((repositoryId != null && repositoryIds.contains(repositoryId) && storagetIds.contains(storageId1))
                    || (repositoryId == null && storagetIds.contains(storageId1))
                    || (StringUtils.isNotEmpty(apiAuthoritie) && apis.contains(apiAuthoritie))
                    || (StringUtils.isNotEmpty(path) && paths.contains(path)));
        }).collect(Collectors.toList());
    }

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    public FolibRole queryById(String id){
        return folibRoleMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param folibRole 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    public Page<FolibRole> paginQuery(FolibRole folibRole, PageRequest pageRequest){
        long total = folibRoleMapper.count(folibRole);
        return new PageImpl<>(folibRoleMapper.queryAllByLimit(folibRole, pageRequest), pageRequest, total);
    }
    
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    public FolibRole insert(FolibRole folibRole){
        folibRoleMapper.insert(folibRole);
        return folibRole;
    }
    
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    public FolibRole update(FolibRole folibRole){
        folibRoleMapper.update(folibRole);
        return queryById(folibRole.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    public boolean deleteById(String id){
        int update = folibRoleMapper.update(FolibRole.builder().id(id).deleted(GlobalConstants.DELETED).build());
        return update > 0;
    }

    @Override
    public void save(RoleDTO roleForm, String username) {

        AccessModelDTO accessModel = roleForm.getAccessModel();
        if(Objects.isNull(accessModel)){
            throw new RuntimeException("权限配置不能为空");
        }
        //保存角色信息
        FolibRole folibRole = queryById(roleForm.getName());
        if (folibRole != null) {
            return;
        }
        Date date = new Date();
        FolibRole roleInfo = FolibRole.builder().id(roleForm.getName()).enName(roleForm.getName()).cnName(roleForm.getDescription()).description(roleForm.getDescription()).createTime(date).createBy(username).updateBy(username).updateTime(date).build();
        insert(roleInfo);
        String roleId = roleInfo.getId();
        //保存权限关系
        savePermissions(roleForm, accessModel, roleId);
    }

    private void savePermissions(RoleDTO roleForm, AccessModelDTO accessModel, String roleId) {
        //保存权限
        List<RoleResourceRef> roleResourceRefs = new ArrayList<>();
        //用户权限组装
        roleForm.getUserIds().forEach(userId -> {
            accessModel.getApiAccess().forEach(privilege -> {
                RoleResourceRef roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).resourceType(GlobalConstants.RESOURCE_TYPE_API).resourceId(privilege.getResourceId()).build();
                roleResourceRefs.add(roleResourceRef);
            });
            accessModel.getRepositoriesAccess().forEach(repositoryAccess -> {
                getRepositoriesAcces(repositoryAccess, roleId, userId, GlobalConstants.ROLE_TYPE_USER, roleResourceRefs);
            });
        });
        //用户组权限组装
        roleForm.getUserGroupIds().forEach(groupId -> {
            accessModel.getApiAccess().forEach(privilege -> {
                RoleResourceRef roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(String.valueOf(groupId)).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).resourceType(GlobalConstants.RESOURCE_TYPE_API).resourceId(privilege.getResourceId()).build();
                roleResourceRefs.add(roleResourceRef);
            });
            accessModel.getRepositoriesAccess().forEach(repositoryAccess -> {
                getRepositoriesAcces(repositoryAccess, roleId, String.valueOf(groupId), GlobalConstants.ROLE_TYPE_USER_GROUP, roleResourceRefs);
            });
        });
        roleResourceRefService.saveBath(roleResourceRefs);
    }

    @Override
    public void updateRoleInfo(RoleDTO roleDTO, String username) {
        AccessModelDTO accessModel = roleDTO.getAccessModel();
        if(Objects.isNull(accessModel)){
            throw new RuntimeException("权限配置不能为空");
        }
        //保存角色信息
        FolibRole folibRole = queryById(roleDTO.getName());
        if (folibRole != null) {
            return;
        }
        Date date = new Date();
        FolibRole roleInfo = FolibRole.builder().id(roleDTO.getName()).enName(roleDTO.getName()).description(roleDTO.getDescription()).updateBy(username).updateTime(date).build();
        update(roleInfo);
        String roleId = roleInfo.getId();
        roleResourceRefService.deleteByRoleId(roleId);
        //保存权限关系
        savePermissions(roleDTO, accessModel, roleId);
    }

    @Override
    public List<FolibRole> queryRoles(FolibRole build) {
        return folibRoleMapper.select(build);
    }

    /**
     * 获取仓库权限
     * @param repositoryAccess 仓库权限req
     * @param roleId 角色id
     * @param enetityId 用户、用户组id
     * @param roleType 角色类型
     * @param roleResourceRefs 角色权限关联列表
     */
    private void getRepositoriesAcces(RepositoryAccessModelDTO repositoryAccess, String roleId, String enetityId, String roleType, List<RoleResourceRef> roleResourceRefs) {
        String path = repositoryAccess.getPath();
        String repositoryId = repositoryAccess.getRepositoryId();
        String resourceType;
        if (StringUtils.isNotEmpty(path)) {
            resourceType = GlobalConstants.RESOURCE_TYPE_PATH;
        } else if (StringUtils.isNoneEmpty(repositoryId)) {
            resourceType = GlobalConstants.RESOURCE_TYPE_REPOSITORY;
        } else {
            resourceType = GlobalConstants.RESOURCE_TYPE_STORAGE;
        }
        repositoryAccess.getPrivileges().forEach(privilege -> {
            RoleResourceRef roleResourceRef;
            if (StringUtils.isNotEmpty(path)) {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).pathPrivilege(privilege).build();
            } else if (StringUtils.isNoneEmpty(repositoryId)) {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).repositoryPrivilege(privilege).build();
            } else {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).storageProvilege(privilege).build();
            }
            roleResourceRefs.add(roleResourceRef);
        });
    }
}