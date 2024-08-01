package com.veadan.folib.users.service.impl;

import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.dto.PermissionsDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.dto.UserRoleDTO;
import com.veadan.folib.entity.RoleResourceRef;
import com.veadan.folib.mapper.RoleResourceRefMapper;
import com.veadan.folib.users.service.RoleResourceRefService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

/**
 * 权限表;(role_resource_ref)表服务实现类
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
@Service
public class RoleResourceRefServiceImpl implements RoleResourceRefService {
    @Autowired
    private RoleResourceRefMapper roleResourceRefMapper;
    
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
    public List<PermissionsDTO> queryPermissions(String roleId) {
        return roleResourceRefMapper.queryPermissions(roleId);
    }
}