package com.veadan.folib.users.service;

import com.veadan.folib.dto.RoleResourceRefDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.UserGroupRef;

import java.util.List;

/**
 * 用户组关联表;(user_group_ref)表服务接口
 * @author : Fengmaogen
 * @date : 2024-7-18
 */
public interface UserGroupRefService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroupRef queryById(Long id);
    /** 
     * 分页查询
     *
     * @param userGroupRef 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    Page<UserGroupRef> paginQuery(UserGroupRef userGroupRef, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    UserGroupRef insert(UserGroupRef userGroupRef);
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    UserGroupRef update(UserGroupRef userGroupRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Long id);
    /**
     * 批量保存用户组关系
     * @param entities
     * @return
     */
    int saveBath(List<UserGroupRef> entities);

    RoleResourceRefDTO queryPrivilegeByGroup(Long groupId, String refType, List<String> roleIds);

    void deleteByUserGroupId(Long id);
}