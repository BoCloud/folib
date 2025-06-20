package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncCornJobEnum;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;

public class SyncCronJobDto {
    private CronTaskConfigurationDto configurationDto;
    private SyncCornJobEnum syncCornJobEnum;

    public SyncCronJobDto() {
    }

    public SyncCronJobDto(CronTaskConfigurationDto configurationDto, SyncCornJobEnum syncCornJobEnum) {
        this.configurationDto = configurationDto;
        this.syncCornJobEnum = syncCornJobEnum;
    }

    public CronTaskConfigurationDto getConfigurationDto() {
        return configurationDto;
    }

    public void setConfigurationDto(CronTaskConfigurationDto configurationDto) {
        this.configurationDto = configurationDto;
    }

    public SyncCornJobEnum getSyncCornJobEnum() {
        return syncCornJobEnum;
    }

    public void setSyncCornJobEnum(SyncCornJobEnum syncCornJobEnum) {
        this.syncCornJobEnum = syncCornJobEnum;
    }
}
