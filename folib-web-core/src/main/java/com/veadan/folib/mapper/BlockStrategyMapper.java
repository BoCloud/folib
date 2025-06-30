package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.domain.blockstrategy.BlockStrategyRecord;
import com.veadan.folib.entity.BlockStrategy;
import com.veadan.folib.forms.blockstrategy.BlockStrategyForm;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface BlockStrategyMapper extends BaseMapper<BlockStrategy> {

    /**
     * 查询阻断策略列表
     * @param blockStrategyForm 参数
     * @return 阻断策略列表
     */
    List<BlockStrategy> selectBlockList(@Param("blockStrategy") BlockStrategyForm blockStrategyForm);

    /**
     * 查询阻断策略列表
     * @param blockStrategyForm 参数
     * @return 阻断策略列表
     */
    List<BlockStrategyRecord> selectInfoList(@Param("blockStrategy") BlockStrategyForm blockStrategyForm);
}
