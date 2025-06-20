package com.veadan.folib.services;

import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.entity.BackupStrategy;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.dto.backupstrategy.BackupStrategyDto;
import java.util.List;

/**
 * @author leipenghui
 **/
public interface BackupStrategyService {


    /**
     * 查询备份策略分页列表
     *
     * @param page              页码
     * @param limit             每页大小
     * @param backupStrategyForm 表单参数
     * @return 备份策略分页列表
     */
    TableResultResponse<BackupStrategyRecord> queryBackupStrategyPage(Integer page, Integer limit, BackupStrategyDto backupStrategyForm);

    /**
     * 查询备份策略列表
     *
     * @param backupStrategyForm 表单参数
     * @return 备份策略分页列表
     */
    List<BackupStrategyRecord> queryBackupStrategyList(BackupStrategyDto backupStrategyForm);

    /**
     * 查询备份策略
     *
     * @param backupStrategy 备份策略
     * @return 备份策略
     */
    BackupStrategyDto queryBackupStrategy(BackupStrategy backupStrategy);

    /**
     * 新增备份策略
     *
     * @param backupStrategyForm 参数
     */
    void saveBackupStrategy(BackupStrategyDto backupStrategyForm);

    /**
     * 更新备份策略
     *
     * @param backupStrategyForm 参数
     */
    void updateBackupStrategy(BackupStrategyDto backupStrategyForm);

    /**
     * 删除备份策略
     *
     * @param backupStrategy 参数
     */
    void deleteBackupStrategy(BackupStrategy backupStrategy);

    /**
     * 备份策略
     *
     * @param backupStrategy 参数
     * @return 备份策略
     */
    BackupStrategy getBackupStrategy(BackupStrategy backupStrategy);

    /**
     * 获取仓库的备份策略缓存
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     * @return 仓库的备份策略缓存
     */
    List<BackupStrategyRecord> getBackupStrategyRecordCache(String storageId, String repositoryId);

    /**
     * 执行备份
     * @param backupStrategyForm 备份策略
     */
    void executeBackup(BackupStrategyDto backupStrategyForm);
}
