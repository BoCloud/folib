package com.veadan.folib.services;

import com.veadan.folib.domain.blockstrategy.BlockStrategyRecord;
import com.veadan.folib.entity.BlockStrategy;
import com.veadan.folib.dto.blockstrategy.BlockStrategyDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface BlockStrategyService {


    /**
     * 查询阻断策略分页列表
     *
     * @param page              页码
     * @param limit             每页大小
     * @param blockStrategyForm 表单参数
     * @return 阻断策略分页列表
     */
    TableResultResponse<BlockStrategyRecord> queryBlockStrategyPage(Integer page, Integer limit, BlockStrategyDto blockStrategyForm);

    /**
     * 查询阻断策略列表
     *
     * @param blockStrategyForm 表单参数
     * @return 阻断策略分页列表
     */
    List<BlockStrategyRecord> queryBlockStrategyList(BlockStrategyDto blockStrategyForm);

    /**
     * 查询阻断策略
     *
     * @param blockStrategy 阻断策略
     * @return 阻断策略
     */
    BlockStrategyDto queryBlockStrategy(BlockStrategy blockStrategy);

    /**
     * 新增阻断策略
     *
     * @param blockStrategyForm 参数
     */
    void saveBlockStrategy(BlockStrategyDto blockStrategyForm);

    /**
     * 更新阻断策略
     *
     * @param blockStrategyForm 参数
     */
    void updateBlockStrategy(BlockStrategyDto blockStrategyForm);

    /**
     * 删除阻断策略
     *
     * @param blockStrategy 参数
     */
    void deleteBlockStrategy(BlockStrategy blockStrategy);

    /**
     * 阻断策略
     *
     * @param blockStrategy 参数
     * @return 阻断策略
     */
    BlockStrategy getBlockStrategy(BlockStrategy blockStrategy);

    /**
     * 获取仓库的阻断策略缓存
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     * @return 仓库的阻断策略缓存
     */
    List<BlockStrategyRecord> getBlockStrategyRecordCache(String storageId, String repositoryId);
}
