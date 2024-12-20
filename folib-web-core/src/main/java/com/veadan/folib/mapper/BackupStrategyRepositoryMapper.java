package com.veadan.folib.mapper;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.entity.BackupStrategyRepository;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface BackupStrategyRepositoryMapper extends CommonMapper<BackupStrategyRepository> {

    /**
     * 批量保存
     *
     * @param backupStrategyRepositoryList 数据
     */
    void batchInsertBackupStrategyRepository(@Param("backupStrategyRepositoryList") List<BackupStrategyRepository> backupStrategyRepositoryList);
}
