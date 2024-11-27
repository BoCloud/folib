package com.veadan.folib.mapper;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.entity.FederalRepository;
import com.veadan.folib.entity.PromotionRule;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 联邦晋级规则;(promotion_rule)表数据库访问层
 *
 * @author : pj
 * @date : 2024-11-21
 */
@Mapper
public interface PromotionRuleMapper  {
    /**
     * 通过ID查询单条数据
     *
     * @param ruleId 主键
     * @return 实例对象
     */
    PromotionRule queryById(long ruleId);


    /**
     * 统计总行数
     *
     * @param promotionRule 查询条件
     * @return 总行数
     */
    long count(PromotionRule promotionRule);

    /**
     * 新增数据
     *
     * @param promotionRule 实例对象
     * @return 影响行数
     */
    int insert(PromotionRule promotionRule);

    /**
     * 批量新增数据
     *
     * @param entities List<PromotionRule> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<PromotionRule> entities);

    /**
     * 批量新增或按主键更新数据
     *
     * @param entities List<PromotionRule> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<PromotionRule> entities);

    /**
     * 更新数据
     *
     * @param promotionRule 实例对象
     * @return 影响行数
     */
    int update(PromotionRule promotionRule);

    /**
     * 通过主键删除数据
     *
     * @param ruleId 主键
     * @return 影响行数
     */
    int deleteById(long ruleId);

    /**
     * 根据策略id删除规则
     * @param policyId 策略ID
     * @return 影响行数
     */
    int deleteByPolicyId(@Param("policyId")long policyId);

    /**
     * 根据策略ID查询规则
     * @param policyId 策略ID
     * @return 联邦晋级策略规则列表
     */
    List<PromotionRule> queryByPolicyId(@Param("policyId")long policyId);
}
