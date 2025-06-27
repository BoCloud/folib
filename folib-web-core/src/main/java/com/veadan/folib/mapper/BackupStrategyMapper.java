package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.entity.BackupStrategy;
import com.veadan.folib.dto.backupstrategy.BackupStrategyDto;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface BackupStrategyMapper extends BaseMapper<BackupStrategy> {

    /**
     * 查询备份策略列表
     *
     * @param backupStrategyForm 参数
     * @return 备份策略列表
     */
    List<BackupStrategyRecord> selectBackupList(@Param("backupStrategy") BackupStrategyDto backupStrategyForm);

    /**
     * 查询备份策略列表
     *
     * @param backupStrategyForm 参数
     * @return 备份策略列表
     */
    List<BackupStrategyRecord> selectInfoList(@Param("backupStrategy") BackupStrategyDto backupStrategyForm);
}
