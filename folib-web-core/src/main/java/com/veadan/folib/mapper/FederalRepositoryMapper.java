package com.veadan.folib.mapper;

import java.util.List;

import com.veadan.folib.entity.FederalRepository;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;


 /**
 * 联邦仓库表;(federal_repository)表数据库访问层
 * @author : pj
 * @date : 2024-11-21
 */
@Mapper
public interface FederalRepositoryMapper{
    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    FederalRepository queryById(long id);
    /**
     * 统计总行数
     *
     * @param federalRepository 查询条件
     * @return 总行数
     */
    long count(FederalRepository federalRepository);
    /**
     * 新增数据
     *
     * @param federalRepository 实例对象
     * @return 影响行数
     */
    int insert(FederalRepository federalRepository);
    /**
     * 批量新增数据
     *
     * @param entities List<FederalRepository> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<FederalRepository> entities);
    /**
     * 批量新增或按主键更新数据
     *
     * @param entities List<FederalRepository> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<FederalRepository> entities);
    /**
     * 更新数据
     *
     * @param federalRepository 实例对象
     * @return 影响行数
     */
    int update(FederalRepository federalRepository);
    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(long id);

     /**
      * 根据策略id删除规则
      * @param policyId 策略ID
      * @return 影响行数
      */
     int deleteByPolicyId(@Param("policyId")long policyId);

     /**
      * 根据策略ID查询规则
      * @param policyId 策略ID
      * @return 联邦仓库列表
      */
     List<FederalRepository> queryByPolicyId(@Param("policyId")long policyId);
}
