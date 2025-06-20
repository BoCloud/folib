package com.veadan.folib.job.cron.jobs;

import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;

import java.util.Collection;

/**
 * @author veadan
 */
public class NoopDuplicationCheckStrategy
        implements CronJobDuplicationCheckStrategy
{

    private static final NoopDuplicationCheckStrategy INSTANCE = new NoopDuplicationCheckStrategy();

    public static NoopDuplicationCheckStrategy getInstance()
    {
        return INSTANCE;
    }


    @Override
    public boolean duplicates(final CronTaskConfigurationDto candidate,
                              final Collection<CronTaskConfigurationDto> existing)
    {
        return false;
    }
}
