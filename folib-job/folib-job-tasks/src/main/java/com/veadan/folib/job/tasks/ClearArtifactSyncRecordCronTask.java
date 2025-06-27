package com.veadan.folib.job.tasks;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.ImmutableSet;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.job.cron.jobs.CronJobDefinition;
import com.veadan.folib.job.cron.jobs.JavaCronJob;
import com.veadan.folib.job.cron.jobs.fields.CronJobField;
import com.veadan.folib.job.cron.jobs.fields.CronJobIntegerTypeField;
import com.veadan.folib.job.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.job.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.job.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.job.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.job.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import jakarta.inject.Inject;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @date 2023/12/6 13:23
 */
public class ClearArtifactSyncRecordCronTask extends JavaCronJob {
    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";
    private static final String RECORD_RETENTION_TIME = "recordRetentionTime";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobIntegerTypeField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(RECORD_RETENTION_TIME))))
    );

    @Inject
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;
    @Inject
    private ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        final String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        final String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        final Long recordRetentionTime = Optional.ofNullable(config.getProperty(RECORD_RETENTION_TIME)).filter(StringUtils::isNotBlank).map(Long::valueOf).orElse(0L);
        final Instant instant = LocalDateTime.now(ZoneId.of("Asia/Shanghai")).minusSeconds(recordRetentionTime).toInstant(ZoneOffset.of("+8"));
        final Date time = Date.from(instant);
        //final List<ArtifactSyncRecord> clearRecordList = artifactSyncRecordMapper.selectClearRecordList(storageId, repositoryId, time);
        regenerateRepositoriesSyncRecord(storageId, repositoryId, time);
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ClearArtifactSyncRecordCronTask.class.getName())
                .name("定时清除晋级/分发记录表数据任务").scope(GLOBAL)
                .description("用于定时定时清除晋级/分发记录表数据，防止记录表数据过大（recordRetentionTime：数据保留时间，单位秒）")
                .fields(FIELDS)
                .build();
    }

    public void regenerateRepositoriesSyncRecord(String storageId, String repositoryId, Date time) {
        List<ArtifactSyncRecord> clearRecordList = artifactSyncRecordMapper.selectClearRecordList(storageId, repositoryId, time);
        if (CollUtil.isNotEmpty(clearRecordList)) {

            if (storageId == null && repositoryId == null) {
                clearRecordList = clearRecordList.stream().filter(s -> !existsRepositoryTask(storageId, repositoryId)).collect(Collectors.toList());
            }
            final List<String> clearSyncNoList = clearRecordList.stream().map(ArtifactSyncRecord::getSyncNo).collect(Collectors.toList());

            // 持久化需要清除clearSyncNoList的进度
            final List<ArtifactSyncSlaveRecord> artifactSyncSlaveRecordList = Optional.ofNullable(artifactSyncSlaveRecordMapper.selectListBySyncNoList(clearSyncNoList))
                    .orElse(Collections.emptyList());
            final Map<String, Map<Integer, Long>> groupSyncNoSlaveRecordCountMap = artifactSyncSlaveRecordList.stream()
                    .collect(Collectors.groupingBy(ArtifactSyncSlaveRecord::getSyncNo,
                            Collectors.groupingBy(ArtifactSyncSlaveRecord::getStatus, Collectors.counting())));

            final ArtifactSyncRecord updater = new ArtifactSyncRecord();
            clearRecordList.forEach(r -> {
                final Map<Integer, Long> groupStatusMap = groupSyncNoSlaveRecordCountMap.getOrDefault(r.getSyncNo(), Collections.emptyMap());
                final int sumCount = groupStatusMap.values().stream().mapToInt(Long::intValue).sum();
                final Long successCount = groupStatusMap.getOrDefault(ArtifactSyncRecordStatusEnum.SUCCESS.getVal(), 0L);
                updater.setId(r.getId());
                updater.setSyncProgress(0D);
                updater.setUpdateTime(new Date());
                if (successCount > 0) {
                    final double syncProgress = BigDecimal.valueOf(successCount).divide(new BigDecimal(sumCount)).setScale(2).doubleValue();
                    updater.setSyncProgress(syncProgress);
                    if (syncProgress >= 1.0D) {
                        updater.setStatus(ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
                    }
                }
                artifactSyncRecordMapper.update(Wrappers.<ArtifactSyncRecord>lambdaUpdate()
                        .eq(ArtifactSyncRecord::getId, r.getId())
                        .set(ArtifactSyncRecord::getSyncProgress, updater.getSyncProgress())
                        .set(ArtifactSyncRecord::getStatus, updater.getStatus())
                        .set(ArtifactSyncRecord::getUpdateTime, updater.getUpdateTime())
                        .set(updater.getStatus()!=null,ArtifactSyncRecord::getStatus, updater.getStatus())
                );
            });
            artifactSyncSlaveRecordMapper.batchDeleteBySyncNoList(clearSyncNoList);
            final int clearSlaveRecordCount = clearRecordList.size();
            logger.info("成功定时清理从记录 {} 条", clearSlaveRecordCount);
        }
    }

    private boolean existsRepositoryTask(String storageId, String repositoryId) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        String cronJob = "com.veadan.folib.cron.jobs.cleanup.ClearArtifactSyncRecordCronJob";
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && cronJob.equals(cron.getJobClass()));
    }
}
