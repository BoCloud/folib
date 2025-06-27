package com.veadan.folib.mapper;

import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import com.veadan.folib.entity.RoleUserRef;

 /**
 * 角色用户关联表;(role_user_ref)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Mapper
public interface RoleUserRefMapper{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    RoleUserRef queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param roleUserRef 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    //List<RoleUserRef> queryAllByLimit(RoleUserRef roleUserRef, @Param("pageable") Pageable pageable);
    /** 
     * 统计总行数
     *
     * @param roleUserRef 查询条件
     * @return 总行数
     */
    long count(RoleUserRef roleUserRef);
    /** 
     * 新增数据
     *
     * @param roleUserRef 实例对象
     * @return 影响行数
     */
    int insert(RoleUserRef roleUserRef);
    /** 
     * 批量新增数据
     *
     * @param entities List<RoleUserRef> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<RoleUserRef> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<RoleUserRef> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<RoleUserRef> entities);
    /** 
     * 更新数据
     *
     * @param roleUserRef 实例对象
     * @return 影响行数
     */
    int update(RoleUserRef roleUserRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);
}