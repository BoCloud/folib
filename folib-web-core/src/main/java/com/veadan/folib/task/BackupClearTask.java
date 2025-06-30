package com.veadan.folib.task;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.NumberUtil;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.backupstrategy.BackupStrategyRecord;
import com.veadan.folib.forms.backupstrategy.BackupStrategyForm;
import com.veadan.folib.mapper.BackupStrategyMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.FileSystemUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 * 全量备份清理task
 **/
@Slf4j
@Component
@EnableScheduling
public class BackupClearTask {

    @Autowired
    private BackupStrategyMapper backupStrategyMapper;

    @Autowired
    private DistributedLockComponent distributedLockComponent;

    /**
     * 每天凌晨0点检查清理全量备份数据
     */
    @Scheduled(cron = "0 0 0 * * ? ")
    public void fullBackup() {
        String lockName = "BackupClearTask";
        long waitTime = 1L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                String incremental = "incremental";
                List<BackupStrategyRecord> backupStrategyList = backupStrategyMapper.selectBackupList(BackupStrategyForm.builder().build());
                if (CollectionUtils.isNotEmpty(backupStrategyList)) {
                    for (BackupStrategyRecord backupStrategyRecord : backupStrategyList) {
                        try {
                            if (StringUtils.isBlank(backupStrategyRecord.getBackupPath())) {
                                continue;
                            }
                            Path backupPath = Paths.get(backupStrategyRecord.getBackupPath()).resolve(backupStrategyRecord.getStrategyName());
                            if (!Files.exists(backupPath)) {
                                continue;
                            }
                            try (Stream<Path> pathStream = Files.list(backupPath)) {
                                pathStream.filter(path -> Files.isDirectory(path) && !path.getFileName().toString().equals(incremental))
                                        .sorted()
                                        .forEach(path -> execute(backupStrategyRecord, path));
                            } catch (IOException ex) {
                                log.error(ExceptionUtils.getStackTrace(ex));
                            }
                        } catch (Exception ex) {
                            log.error("检查清理全量备份数据，备份策略 [{}] 错误 [{}]", backupStrategyRecord.getStrategyName(), ExceptionUtils.getStackTrace(ex));
                        }
                    }
                }
            } catch (Exception ex) {
                log.error("检查清理全量备份数据定时任务错误 [{}]", ExceptionUtils.getStackTrace(ex));
            } finally {
                distributedLockComponent.unLock(lockName, 1500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("Scheduled backupClearTask end");
    }

    private void execute(BackupStrategyRecord backupStrategyRecord, Path path) {
        try {
            log.info("Backup clear strategyName [{}] path [{}]", backupStrategyRecord.getStrategyName(), path);
            String backupDirectory = path.getFileName().toString();
            Integer retentionPeriod = backupStrategyRecord.getRetentionPeriod();
            if (Objects.isNull(retentionPeriod) || NumberUtil.compare(retentionPeriod, GlobalConstants.ZERO) <= 0) {
                retentionPeriod = 7;
            }
            DateTime backupDateTime = DateUtil.parse(backupDirectory, DatePattern.PURE_DATETIME_PATTERN);
            LocalDateTime backupLocalDateTime = DateUtil.toLocalDateTime(backupDateTime);
            DateTime dateTime = DateUtil.date();
            LocalDateTime localDateTime = DateUtil.toLocalDateTime(dateTime);
            if (!localDateTime.minusDays(retentionPeriod).isBefore(backupLocalDateTime)) {
                FileSystemUtils.deleteRecursively(path);
                log.info("Backup clear strategyName [{}] path [{}] retentionPeriod [{}] expired, successfully deleted", backupStrategyRecord.getStrategyName(), path, retentionPeriod);
            }
        } catch (Exception ex) {
            log.error("Backup clear strategyName [{}] path [{}] error [{}]", backupStrategyRecord.getStrategyName(), path, ExceptionUtils.getStackTrace(ex));
        }
    }
}
