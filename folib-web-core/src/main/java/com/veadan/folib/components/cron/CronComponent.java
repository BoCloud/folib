package com.veadan.folib.components.cron;

import com.veadan.folib.cluster.SyncCornJobEnum;
import com.veadan.folib.controllers.cluster.dto.SyncCronJobDto;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.services.CronTaskConfigurationService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * @author veadan
 * @date 2024/12/18
 **/
@Slf4j
@Component
public class CronComponent {

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    public void configCronTask(String cronName, String className, String cron) {
        configCronTask(cronName, className, cron, null);
    }

    public void configCronTask(String cronName, String className, String cron, Map<String, String> properties) {
        CronTaskConfigurationDto cronTaskConfiguration = new CronTaskConfigurationDto();
        cronTaskConfiguration.setName(cronName);
        cronTaskConfiguration.setJobClass(className);
        cronTaskConfiguration.setCronExpression(cron);
        cronTaskConfiguration.setOneTimeExecution(false);
        cronTaskConfiguration.setImmediateExecution(false);
        if (MapUtils.isNotEmpty(properties)) {
            cronTaskConfiguration.setProperties(properties);
        }
        try {
            deleteTask(cronTaskConfiguration);
            UUID uuid = cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);
            cronTaskConfiguration.setUuid(uuid);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    public void deleteCronTask(String cronName, String className, String cron) {
        try {
            CronTaskConfigurationDto cronTaskConfiguration = new CronTaskConfigurationDto();
            cronTaskConfiguration.setName(cronName);
            cronTaskConfiguration.setJobClass(className);
            cronTaskConfiguration.setCronExpression(cron);
            cronTaskConfiguration.setOneTimeExecution(false);
            cronTaskConfiguration.setImmediateExecution(false);
            deleteTask(cronTaskConfiguration);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    private void deleteTask(CronTaskConfigurationDto cronTaskConfiguration) throws Exception {
        Optional<CronTaskConfigurationDto> cronTaskConfigurationOptional = cronTaskConfigurationService.getTasksConfigurationDto().getCronTaskConfigurations().stream().filter(item -> item.getJobClass().equals(cronTaskConfiguration.getJobClass()) && item.getName().equals(cronTaskConfiguration.getName())).findFirst();
        if (cronTaskConfigurationOptional.isPresent()) {
            CronTaskConfigurationDto cronTaskConfigurationDto = cronTaskConfigurationOptional.get();
            cronTaskConfigurationService.deleteConfiguration(cronTaskConfigurationDto.getUuid());
        }
    }

}
