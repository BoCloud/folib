package com.veadan.folib.cron.jobs;

import cn.hutool.core.collection.CollUtil;
import com.google.common.collect.ImmutableSet;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.cron.jobs.fields.CronJobIntegerTypeField;
import com.veadan.folib.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import org.apache.commons.lang3.StringUtils;

import javax.inject.Inject;
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
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 13:23
 * @since x.x.x
 */
public class ClearArtifactSyncRecordCronJob extends JavaCronJob {
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
        final List<ArtifactSyncRecord> clearRecordList = artifactSyncRecordMapper.selectClearRecordList(storageId, repositoryId, time);
        
        if (CollUtil.isNotEmpty(clearRecordList)) {
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
                artifactSyncRecordMapper.updateByPrimaryKeySelective(updater);
            });

            artifactSyncSlaveRecordMapper.batchDeleteBySyncNoList(clearSyncNoList);
            final int clearSlaveRecordCount = clearRecordList.size(); 
            logger.info("成功定时清理从记录 {} 条", clearSlaveRecordCount);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ClearArtifactSyncRecordCronJob.class.getName())
                .name("定时清除晋级/分发记录表数据任务").scope(GLOBAL)
                .description("用于定时定时清除晋级/分发记录表数据，防止记录表数据过大（recordRetentionTime：数据保留时间，单位秒）")
                .fields(FIELDS)
                .build();
    }
}
