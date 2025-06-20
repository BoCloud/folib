package com.veadan.folib.job.event.cron;

import com.veadan.folib.event.AbstractEventListenerRegistry;

import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author Veadan
 */
@Component
public class CronTaskEventListenerRegistry
        extends AbstractEventListenerRegistry
{

    private static final Logger logger = LoggerFactory.getLogger(CronTaskEventListenerRegistry.class);


    public void dispatchCronTaskCreatedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_SAVED.getType(), uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_SAVED event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskDeletedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_DELETED.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_DELETED event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskExecutingEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTING.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTING event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskExecutedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTION_COMPLETE.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTION_COMPLETE event for '{}'...", uuid);

        dispatchEvent(event);
    }

}
