package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.BlockStrategyRepository;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface BlockStrategyRepositoryMapper extends BaseMapper<BlockStrategyRepository> {

    /**
     * 批量保存
     *
     * @param blockStrategyRepositoryList 数据
     */
    void batchInsertBlockStrategyRepository(@Param("blockStrategyRepositoryList") List<BlockStrategyRepository> blockStrategyRepositoryList);
}
