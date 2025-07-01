package com.veadan.folib.event.repository;

import com.veadan.folib.event.AbstractEventListenerRegistry;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class RepositoryEventListenerRegistry
        extends AbstractEventListenerRegistry
{

    public void dispatchRepoDelteToCronJobDeleteEvent(String storageId,
                                                      String repositoryId) {
        RepositoryEvent event = new RepositoryEvent(storageId,
                repositoryId,
                RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETE_TO_CRON_JOB_DELETED.getType());

        dispatchEvent(event);
    }

    public void dispatchRepoDelteAllToCronJobDeleteEvent(String storageId,
                                                         String repositoryId) {
        RepositoryEvent event = new RepositoryEvent(storageId,
                null,
                RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETE_ALL_TO_CRON_JOB_DELETED.getType());

        dispatchEvent(event);
    }

}
