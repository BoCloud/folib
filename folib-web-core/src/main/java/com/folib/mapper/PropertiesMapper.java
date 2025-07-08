package com.folib.mapper;

import com.folib.entity.PropertiesEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface PropertiesMapper {
    /**
     * 统计总行数
     *
     * @param id 查询条件
     * @return 总行数
     */
    long count(@Param("id") String id);

    /**
     * 新增数据
     *
     * @param properties 实例对象
     * @return 影响行数
     */
    int insert(PropertiesEntity properties);

    /**
     * 批量新增数据
     *
     * @param entities List<Properties> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<PropertiesEntity> entities);

    /**
     * 批量新增或按主键更新数据
     *
     * @param entities List<Properties> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<PropertiesEntity> entities);

    /**
     * 更新数据
     *
     * @param properties 实例对象
     * @return 影响行数
     */
    int update(PropertiesEntity properties);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(String id);
}
