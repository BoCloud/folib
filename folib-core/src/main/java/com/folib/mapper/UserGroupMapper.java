package com.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.dto.UserGroupDTO;
import com.folib.dto.UserGroupListDTO;
import com.folib.entity.UserGroup;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

 /**
 * 用户组;(user_group)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Mapper
public interface UserGroupMapper extends BaseMapper<UserGroup> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroup queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param userGroup 查询条件
     * @return 对象列表
     */
    List<UserGroupListDTO> queryAllByLimit(@Param("userGroup") UserGroup userGroup);

     /**
      * 查询用户组及用户组下的成员
      * @param userGroup 查询条件
      * @return 对象列表
      */
     List<UserGroupListDTO> queryAllByUser(@Param("userGroup") UserGroup userGroup);
    /** 
     * 统计总行数
     *
     * @param userGroup 查询条件
     * @return 总行数
     */
    long count(UserGroup userGroup);
    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 影响行数
     */
    int insert(UserGroup userGroup);
    /** 
     * 批量新增数据
     *
     * @param entities List<UserGroup> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<UserGroup> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<UserGroup> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<UserGroup> entities);
    /** 
     * 更新数据
     *
     * @param userGroup 实例对象
     * @return 影响行数
     */
    int update(UserGroup userGroup);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

     UserGroupDTO queryGroupDetailById(Long groupId);
 }