package com.veadan.folib.mapper;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.entity.Resource;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;

import java.util.List;

 /**
 * 资源表;(resource)表数据库访问层
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Mapper
public interface ResourceMapper extends CommonMapper<Resource> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    Resource queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param resource 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    List<Resource> queryAllByLimit(Resource resource, @Param("pageable") Pageable pageable);
    /** 
     * 统计总行数
     *
     * @param resource 查询条件
     * @return 总行数
     */
    long count(Resource resource);
    /** 
     * 新增数据
     *
     * @param resource 实例对象
     * @return 影响行数
     */
    int insert(Resource resource);
    /** 
     * 批量新增数据
     *
     * @param entities List<Resource> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<Resource> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<Resource> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<Resource> entities);
    /** 
     * 更新数据
     *
     * @param resource 实例对象
     * @return 影响行数
     */
    int update(Resource resource);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

     List<Resource> findResources(@Param("resources") List<Resource> resources);
 }