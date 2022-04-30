package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Przemyslaw Fusik
 */
public class TestJavaCronJob
        extends JavaCronJob
{

    private final Logger logger = LoggerFactory.getLogger(TestJavaCronJob.class);

    @Override
    public void executeTask(final CronTaskConfigurationDto config)
    {
        logger.info("Executing ... ");
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder().jobClass(TestJavaCronJob.class.getName()).build();
    }


}
