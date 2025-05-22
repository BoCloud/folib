package com.veadan.folib.services;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.event.repository.RepositoryEvent;
import com.veadan.folib.event.repository.RepositoryEventTypeEnum;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.util.Set;

@Component
public class StroagesDeleteToCronJobDeleteListener {
    private static final Logger logger = LoggerFactory.getLogger(StroagesDeleteToCronJobDeleteListener.class);

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    @EventListener
    public void handle(RepositoryEvent event) {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETE_ALL_TO_CRON_JOB_DELETED.getType()) {
            return;
        }
        CronTasksConfigurationDto cronTasksConfigurationDto = cronTaskConfigurationService.getTasksConfigurationDto();
        Set<CronTaskConfigurationDto> cronTaskConfigurations = cronTasksConfigurationDto.getCronTaskConfigurations();
        for (CronTaskConfigurationDto configurationDto : cronTaskConfigurations) {
            try {
                String storageId = configurationDto.getProperty("storageId");
                boolean flag = StringUtils.isNotBlank(storageId)
                        && event.getStorageId().equals(storageId);
                if (flag) {
                    cronTaskConfigurationService.deleteConfiguration(configurationDto.getUuid());
                    logger.info("[{}] handle stroages delete delete to cron job delete end", storageId);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}
