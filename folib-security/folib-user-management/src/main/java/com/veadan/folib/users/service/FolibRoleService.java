package com.veadan.folib.users.service;

import com.veadan.folib.dto.FolibRoleDTO;
import com.veadan.folib.dto.RoleDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.FolibRole;

import java.util.List;
import java.util.Set;

/**
 * 角色信息;(folib_role)表服务接口
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
public interface FolibRoleService{
    //通过用户查询用户关联的权限

    //通过角色id查询查询权限
     FolibRole queryByRoleId(List<String> roleIds);
     /**
      * 同步配置权限
      */
     void syncYamlAuthorizationConfig();
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    FolibRole queryById(String id);
    /** 
     * 分页查询
     *
     * @param folibRole 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    Page<FolibRoleDTO> paginQuery(FolibRole folibRole, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    FolibRole insert(FolibRole folibRole);
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    FolibRole update(FolibRole folibRole);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(String id);

    void save(RoleDTO roleDTO, String username);



    void updateRoleInfo(RoleDTO roleDTO, String roleId, String username);

    void deleteUserRoleCache(List<String> userIds);

    List<FolibRole> queryRoles(FolibRole build);

    /**
     * 获取角色详情
     * @param roleId 角色id
     * @param folibRole 角色信息
     * @return 权限信息
     */
    RoleDTO getRoleDetail(String roleId, FolibRole folibRole);

    void deleteRole(String roleId);

    void saveOrUpdateBatch(List<FolibRole> roles);

    List<FolibRole> queryByIds(Set<String> roles);
}