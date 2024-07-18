package com.veadan.folib.users.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.RoleResourceRef;

import java.util.List;

/**
 * 权限表;(role_resource_ref)表服务接口
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
public interface RoleResourceRefService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    RoleResourceRef queryById(String id);
    /** 
     * 分页查询
     *
     * @param roleResourceRef 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    Page<RoleResourceRef> paginQuery(RoleResourceRef roleResourceRef, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    RoleResourceRef insert(RoleResourceRef roleResourceRef);
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    RoleResourceRef update(RoleResourceRef roleResourceRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(String id);

    /**
     * 批量保存用户权限
     * @param roleResourceRefs
     * @return 保存数量
     */
     int saveBath(List<RoleResourceRef> roleResourceRefs);
    /**
     * 列表查询
     *
     * @param roleResourceRef 筛选条件
     * @return 查询结果
     */
    List<RoleResourceRef> queryRefs(RoleResourceRef roleResourceRef);

    /**
     * 根据角色Id查询权限列表
     * @param roleIds
     * @return
     */
    List<RoleResourceRef> queryRefsByRoleIds(List<String> roleIds);
}