package com.folib.users.service;

import com.github.pagehelper.PageInfo;
import com.folib.dto.UserGroupDTO;
import com.folib.dto.UserGroupListDTO;
import com.folib.entity.UserGroup;
import org.springframework.data.domain.PageRequest;

import java.util.List;

/**
 * 用户组;(user_group)表服务接口
 * @author veadan
 * @date : 2024-7-17
 */
public interface UserGroupService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroup queryById(Long id);
    /** 
     * 分页查询
     *
     * @param userGroup 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    PageInfo<UserGroupListDTO> paginQuery(UserGroup userGroup, PageRequest pageRequest);

    PageInfo<UserGroupListDTO> pageQueryAndUserNumber(UserGroup userGroup, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    UserGroup save(UserGroup userGroup);
    /** 
     * 更新数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    UserGroup update(UserGroup userGroup);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Long id);

    /**
     * 列表查询
     *
     * @param userGroup   筛选条件
     * @return 查询结果
     */
    List<UserGroup> queryUserGroupList(UserGroup userGroup);

    List<UserGroup> findAll();

    UserGroupDTO queryGroupDetailById(Long groupId);

    void saveOrUpdateBatch(List<UserGroup> groups);

    List<UserGroup> queryByIds(List<Long> ids);

    List<UserGroup> queryByGroupNames(List<String> groupNames);
}