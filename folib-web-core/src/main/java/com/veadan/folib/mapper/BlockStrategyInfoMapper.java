package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.BlockStrategyInfo;
import com.veadan.folib.entity.BlockStrategyRepository;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface BlockStrategyInfoMapper extends BaseMapper<BlockStrategyInfo> {

    /**
     * 批量保存
     *
     * @param blockStrategyInfoList 数据
     */
    void batchInsertBlockStrategyInfo(@Param("blockStrategyInfoList") List<BlockStrategyInfo> blockStrategyInfoList);

}
