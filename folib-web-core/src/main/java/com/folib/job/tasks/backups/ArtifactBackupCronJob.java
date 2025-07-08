package com.folib.job.tasks.backups;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.components.backup.BackupComponent;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Map;

/**
 * @author veadan
 **/
@Slf4j
public class ArtifactBackupCronJob extends JavaCronJob {

    public static final String PROPERTY_BACKUP_PATH = "backupPath";

    public static final String PROPERTY_STRATEGY_NAME = "strategyName";

    public static final String PROPERTY_STORAGE_ID = "storageId";

    public static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    public static final String PROPERTY_INCREMENTAL = "incremental";

    @Autowired
    private BackupComponent backupComponent;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        String strategyName = config.getProperty(PROPERTY_STRATEGY_NAME);
        String backupPath = config.getProperty(PROPERTY_BACKUP_PATH);
        String incremental = config.getProperty(PROPERTY_INCREMENTAL);
        if (StringUtils.isBlank(backupPath)) {
            return;
        }
        Map<String, String> properties = config.getProperties();
        Map<String, String> propertiesMap = Maps.newLinkedHashMap(properties);
        propertiesMap.remove(PROPERTY_STRATEGY_NAME);
        propertiesMap.remove(PROPERTY_BACKUP_PATH);
        propertiesMap.remove(PROPERTY_STORAGE_ID);
        propertiesMap.remove(PROPERTY_REPOSITORY_ID);
        propertiesMap.remove(PROPERTY_INCREMENTAL);
        if (MapUtils.isEmpty(propertiesMap)) {
            //全量备份
            return;
        }
        backupComponent.backupRepositories(strategyName, backupPath, Boolean.TRUE.equals(Boolean.valueOf(incremental)), Lists.newLinkedList(propertiesMap.keySet()));
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ArtifactBackupCronJob.class.getName())
                .name("定时全量备份制品的任务")
                .description("定时全量备份制品的任务")
                .build();
    }

}
