package com.veadan.folib.users.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.FolibRole;

import java.util.Collection;
import java.util.List;

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
    Page<FolibRole> paginQuery(FolibRole folibRole, PageRequest pageRequest);
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
}