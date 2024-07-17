package com.veadan.folib.users.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import com.veadan.folib.entity.UserGroup;

 /**
 * 用户组;(user_group)表服务接口
 * @author : http://www.chiner.pro
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
    Page<UserGroup> paginQuery(UserGroup userGroup, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    UserGroup insert(UserGroup userGroup);
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
}