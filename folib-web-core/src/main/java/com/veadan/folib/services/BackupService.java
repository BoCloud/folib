package com.veadan.folib.services;

import com.veadan.folib.dto.backup.BackupDto;

/**
 * @author leipenghui
 * @date 2023/9/26
 **/
public interface BackupService {

    /**
     * 保存备份策略
     *
     * @param backupForm 备份策略
     */
    void saveBackup(BackupDto backupForm);
}
