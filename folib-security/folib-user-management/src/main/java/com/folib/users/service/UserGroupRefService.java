package com.folib.users.service;

import com.folib.dto.RoleResourceRefDTO;
import com.folib.entity.UserGroupRef;

import java.util.List;

/**
 * 用户组关联表;(user_group_ref)表服务接口
 * @author veadan
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
    ///**
    // * 分页查询
    // *
    // * @param userGroupRef 筛选条件
    // * @param pageRequest 分页对象
    // * @return 查询结果
    // */
    //IPage<UserGroupRef> paginQuery(UserGroupRef userGroupRef, PageRequest pageRequest);
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

    List<RoleResourceRefDTO> queryPrivilegeByGroup(Long groupId, String refType, List<String> roleIds);

    void deleteByUserGroupId(Long id);
    void deleteByUserId(String userId);

    void batchUpdate(List<UserGroupRef> userGroups);

    List<UserGroupRef> queryByGroupIds(List<Long> groupIds);
    List<UserGroupRef> queryByUserId(String userId);

    void deleteByIds(List<Long> refIds);
}