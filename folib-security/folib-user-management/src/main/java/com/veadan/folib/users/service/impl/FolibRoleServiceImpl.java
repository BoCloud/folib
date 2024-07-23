package com.veadan.folib.users.service.impl;

import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.constant.GlobalConstants;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
                List<Resource> resources = new ArrayList<>();

                roles.forEach(roleDto -> {
                    folibRoles.add(FolibRole.builder().id(roleDto.getName()).description(roleDto.getDescription())
                            .enName(roleDto.getName()).deleted(GlobalConstants.NOT_DELETED).isDefault(GlobalConstants.NOT_DEFALUT).cnName(roleDto.getDescription()).build());
                    AccessModelDto accessModel = roleDto.getAccessModel();
                    if(accessModel != null) {
                        accessModel.getApiAuthorities().forEach(privileges -> {
                            resources.add(Resource.builder().apiAuthoritie(privileges.getAuthority()).build());
                            apiUserPrivilegeRoles.add(RoleResourceRef.builder().roleId(roleDto.getName()).resourceType(GlobalConstants.RESOURCE_TYPE_API).apiAuthoritie(privileges.getAuthority()).build());
                        });
                        accessModel.getStorageAuthorities().forEach(storagePrivilegesDto -> {
                            resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).build());
                            Set<Privileges> storagePrivileges = storagePrivilegesDto.getStoragePrivileges();
                            if(CollectionUtils.isNotEmpty(storagePrivileges)){
                                List<RoleResourceRef> storagePrivilegeRef = storagePrivileges.stream().map(privilege ->
                                        RoleResourceRef.builder().roleId(roleDto.getName()).storageId(storagePrivilegesDto.getStorageId()).storageProvilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
                                storagePrivilegeRoles.addAll(storagePrivilegeRef);

                                Set<RepositoryPrivilegesDto> repositorytories = storagePrivilegesDto.getRepositoryPrivileges();
                                repositorytories.forEach(repositoryPrivilegesDto -> {
                                    resources.add(Resource.builder().repositoryId(repositoryPrivilegesDto.getRepositoryId()).build());
                                    Set<Privileges> repositoryPrivileges = repositoryPrivilegesDto.getRepositoryPrivileges();
                                    List<RoleResourceRef> repositoryRef = repositoryPrivileges.stream().map(privilege ->
                                            RoleResourceRef.builder().roleId(roleDto.getName()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).repositoryPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build()).collect(Collectors.toList());
                                    repositoryPrivilegeRoles.addAll(repositoryRef);

                                    repositoryPrivilegesDto.getPathPrivileges().forEach(pathPrivilegesDto -> {
                                        resources.add(Resource.builder().path(pathPrivilegesDto.getPath()).build());
                                        Set<Privileges> privileges = pathPrivilegesDto.getPrivileges();
                                        List<RoleResourceRef> pathRef = privileges.stream().map(privilege ->
                                                RoleResourceRef.builder().roleId(roleDto.getName()).path(pathPrivilegesDto.getPath()).pathPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build()).collect(Collectors.toList());
                                        pathPrivilegeRoles.addAll(pathRef);
                                    });
                                });
                            }
                        });
                    }
                });
                //角色入库
                if(CollectionUtils.isNotEmpty(folibRoles)){
                    folibRoleMapper.insertOrUpdateBatch(folibRoles);
                }
                //资源入库
                if(CollectionUtils.isNotEmpty(resources)) {
                    resourceService.saveBatch(resources.stream().distinct().collect(Collectors.toList()));

                    Map<String, Resource> pathMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getPath())).collect(Collectors.toMap(Resource::getPath, resource -> resource, (k1, k2)->k1));
                    Map<String, Resource> storageMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getStorageId())).collect(Collectors.toMap(Resource::getStorageId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> repositoryMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getRepositoryId())).collect(Collectors.toMap(Resource::getRepositoryId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> apiMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getApiAuthoritie())).collect(Collectors.toMap(Resource::getApiAuthoritie, resource -> resource, (k1,k2)->k1));
                    //权限入库
                    List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(folibRoles.stream().map(FolibRole::getId).collect(Collectors.toList()));
                    Map<String, List<RoleResourceRef>> userRoles = roleResourceRefs.stream().collect(Collectors.groupingBy(RoleResourceRef::getRoleId));

                    List<RoleResourceRef> storageRef = storagePrivilegeRoles.stream().peek(roleResourceRef -> {
                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                                if (storageMap.containsKey(roleResourceRef.getStorageId())) {
                                    roleResourceRef.setResourceId(storageMap.get(roleResourceRef.getStorageId()).getId());
                                }
                            });
                        }
                    }).collect(Collectors.toList());
                    if(CollectionUtils.isNotEmpty(storageRef)) {
                        roleResourceRefService.saveBath(storageRef);
                    }
                    List<RoleResourceRef> repositoryRef = repositoryPrivilegeRoles.stream().peek(roleResourceRef -> {
                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                                if (repositoryMap.containsKey(roleResourceRef.getRepositoryId())) {
                                    roleResourceRef.setResourceId(repositoryMap.get(roleResourceRef.getRepositoryId()).getId());
                                }
                            });
                        }
                    }).collect(Collectors.toList());
                    if(CollectionUtils.isNotEmpty(repositoryRef)){
                        roleResourceRefService.saveBath(repositoryRef);
                    }
                    List<RoleResourceRef> pathRef = pathPrivilegeRoles.stream().peek(roleResourceRef -> {
                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                                if (pathMap.containsKey(roleResourceRef.getPath())) {
                                    roleResourceRef.setResourceId(pathMap.get(roleResourceRef.getPath()).getId());
                                }
                            });
                        }
                    }).collect(Collectors.toList());
                    if(CollectionUtils.isNotEmpty(pathRef)){
                        roleResourceRefService.saveBath(pathRef);
                    }
                    List<RoleResourceRef> apiRef = apiUserPrivilegeRoles.stream().peek(roleResourceRef -> {
                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_API);
                                roleResourceRef.setRefType(ref.getRefType());
                                if (apiMap.containsKey(roleResourceRef.getApiAuthoritie())) {
                                    roleResourceRef.setResourceId(apiMap.get(roleResourceRef.getApiAuthoritie()).getId());
                                }
                            });
                        }
                    }).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(apiRef)) {
                        roleResourceRefService.saveBath(apiRef);
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
        int total = folibRoleMapper.deleteById(id);
        return total > 0;
    }
}