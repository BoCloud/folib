package com.veadan.folib.mapper;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.entity.BackupStrategy;
import com.veadan.folib.forms.backupstrategy.BackupStrategyForm;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface BackupStrategyMapper extends CommonMapper<BackupStrategy> {

    /**
     * 查询备份策略列表
     *
     * @param backupStrategyForm 参数
     * @return 备份策略列表
     */
    List<BackupStrategyRecord> selectList(@Param("backupStrategy") BackupStrategyForm backupStrategyForm);

    /**
     * 查询备份策略列表
     *
     * @param backupStrategyForm 参数
     * @return 备份策略列表
     */
    List<BackupStrategyRecord> selectInfoList(@Param("backupStrategy") BackupStrategyForm backupStrategyForm);
}
