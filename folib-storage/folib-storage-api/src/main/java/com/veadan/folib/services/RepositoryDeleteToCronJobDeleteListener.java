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

import javax.inject.Inject;
import java.util.Set;

@Component
public class RepositoryDeleteToCronJobDeleteListener {
    private static final Logger logger = LoggerFactory.getLogger(RepositoryDeleteToCronJobDeleteListener.class);

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    @EventListener
    public void handle(RepositoryEvent event) {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETE_TO_CRON_JOB_DELETED.getType()) {
            return;
        }
        CronTasksConfigurationDto cronTasksConfigurationDto = cronTaskConfigurationService.getTasksConfigurationDto();
        Set<CronTaskConfigurationDto> cronTaskConfigurations = cronTasksConfigurationDto.getCronTaskConfigurations();
        for (CronTaskConfigurationDto configurationDto : cronTaskConfigurations) {
            try {
                String storageId = configurationDto.getProperty("storageId");
                String repositoryId = configurationDto.getProperty("repositoryId");
                boolean flag = StringUtils.isNotBlank(storageId) &&
                        StringUtils.isNotBlank(repositoryId)
                        && event.getStorageId().equals(storageId)
                        && event.getRepositoryId().equals(repositoryId);
                if (flag) {
                    cronTaskConfigurationService.deleteConfiguration(configurationDto.getUuid());
                    logger.info("[{} {}] handle repository delete to cron job delete end", storageId, repositoryId);
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}
