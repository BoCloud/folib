package com.veadan.folib.mapper;

import java.util.List;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.dto.FolibRoleDTO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import com.veadan.folib.entity.FolibRole;

 /**
 * 角色信息;(folib_role)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Mapper
public interface FolibRoleMapper extends BaseMapper<FolibRole> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    FolibRole queryById(String id);
    /** 
     * 分页查询指定行数据
     *
     * @param folibRole 查询条件
     * @return 对象列表
     */
    List<FolibRoleDTO> queryAllByLimit(@Param(("role")) FolibRole folibRole);
    /** 
     * 统计总行数
     *
     * @param folibRole 查询条件
     * @return 总行数
     */
    long count(FolibRole folibRole);
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 影响行数
     */
    int insert(FolibRole folibRole);
    /** 
     * 批量新增数据
     *
     * @param entities List<FolibRole> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<FolibRole> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<FolibRole> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<FolibRole> entities);
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 影响行数
     */
    int update(FolibRole folibRole);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(String id);
}