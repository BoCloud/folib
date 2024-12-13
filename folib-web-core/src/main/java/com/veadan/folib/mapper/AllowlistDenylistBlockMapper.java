package com.veadan.folib.mapper;


import java.util.List;

import com.veadan.folib.entity.AllowlistDenylistBlock;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;

/**
 * 黑白名单阻断;(allowlist_denylist_block)表数据库访问层
 *
 * @author : pj
 * @date : 2024-12-6
 */
@Mapper
public interface AllowlistDenylistBlockMapper {
    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    AllowlistDenylistBlock queryById(Integer id);

    /**
     * 分页查询指定行数据
     *
     * @param allowlistDenylistBlock 查询条件
     * @param pageable               分页对象
     * @return 对象列表
     */
    List<AllowlistDenylistBlock> queryAllByLimit(@Param("entity") AllowlistDenylistBlock allowlistDenylistBlock, @Param("pageable") Pageable pageable);

    /**
     * 统计总行数
     *
     * @param allowlistDenylistBlock 查询条件
     * @return 总行数
     */
    long count(AllowlistDenylistBlock allowlistDenylistBlock);

    /**
     * 新增数据
     *
     * @param allowlistDenylistBlock 实例对象
     * @return 影响行数
     */
    int insert(AllowlistDenylistBlock allowlistDenylistBlock);

    /**
     * 批量新增数据
     *
     * @param entities List<AllowlistDenylistBlock> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<AllowlistDenylistBlock> entities);

    /**
     * 批量新增或按主键更新数据
     *
     * @param entities List<AllowlistDenylistBlock> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<AllowlistDenylistBlock> entities);

    /**
     * 更新数据
     *
     * @param allowlistDenylistBlock 实例对象
     * @return 影响行数
     */
    int update(AllowlistDenylistBlock allowlistDenylistBlock);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Integer id);

    /**
     * 删除黑白名单阻断
     *
     * @param allowlistDenylistBlock 黑白名单参数
     * @return int
     */
    int deleteAllowlistDenylistBloc(@Param("entity") AllowlistDenylistBlock allowlistDenylistBlock);

    /**
     * 查询黑白名单阻断
     * @param allowlistDenylistBlock 黑白名单参数
     * @return 黑白名单阻断
     */
    AllowlistDenylistBlock queryAllowlistDenylistBlock(AllowlistDenylistBlock allowlistDenylistBlock);
}
