package com.veadan.folib.job.cron.jobs;

import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;

import java.util.Collection;

/**
 * @author veadan
 */
public interface CronJobDuplicationCheckStrategy
{

    /**
     * Checks whether the `candidate` should be considered as a duplicate
     * of one of the elements from provided `existing` collection
     */
    boolean duplicates(CronTaskConfigurationDto candidate,
                       Collection<CronTaskConfigurationDto> existing);
}
