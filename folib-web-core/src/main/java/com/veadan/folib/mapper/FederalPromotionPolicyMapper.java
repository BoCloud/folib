package com.veadan.folib.mapper;


import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.entity.FederalPromotionPolicy;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;

import java.util.List;

/**
 * 联邦晋级策略表;(federal_promotion_policy)表数据库访问层
 *
 * @date : 2024-11-21
 */
@Mapper
public interface FederalPromotionPolicyMapper  {
    /**
     * 通过ID查询单条数据
     *
     * @param policyId 主键
     * @return 实例对象
     */
    FederalPromotionPolicy queryById(long policyId);

    /**
     * 统计总行数
     *
     * @param federalPromotionPolicy 查询条件
     * @return 总行数
     */
    long count(FederalPromotionPolicy federalPromotionPolicy);

    /**
     * 新增数据
     *
     * @param federalPromotionPolicy 实例对象
     * @return 影响行数
     */
    int insert(FederalPromotionPolicy federalPromotionPolicy);

    /**
     * 批量新增数据
     *
     * @param entities List<FederalPromotionPolicy> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<FederalPromotionPolicy> entities);

    /**
     * 批量新增或按主键更新数据
     *
     * @param entities List<FederalPromotionPolicy> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<FederalPromotionPolicy> entities);

    /**
     * 更新数据
     *
     * @param federalPromotionPolicy 实例对象
     * @return 影响行数
     */
    int update(FederalPromotionPolicy federalPromotionPolicy);

    /**
     * 通过主键删除数据
     *
     * @param policyId 主键
     * @return 影响行数
     */
    int deleteById(long policyId);

    /**
     * 根据名称查询
     * @param name 策略名
     * @return List<FederalPromotionPolicy>
     */
    List<FederalPromotionPolicy> queryByName(@Param("name") String name);

    /**
     * 分页查询指定行数据
     *
     * @param federalPromotionPolicy 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    List<FederalPromotionPolicy> queryAllByLimit(@Param("policy") FederalPromotionPolicy federalPromotionPolicy, @Param("pageable") Pageable pageable);
}