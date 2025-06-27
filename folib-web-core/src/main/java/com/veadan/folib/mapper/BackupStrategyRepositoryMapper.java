package com.veadan.folib.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.BackupStrategyRepository;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface BackupStrategyRepositoryMapper extends BaseMapper<BackupStrategyRepository> {

    /**
     * 批量保存
     *
     * @param backupStrategyRepositoryList 数据
     */
    void batchInsertBackupStrategyRepository(@Param("backupStrategyRepositoryList") List<BackupStrategyRepository> backupStrategyRepositoryList);
}
