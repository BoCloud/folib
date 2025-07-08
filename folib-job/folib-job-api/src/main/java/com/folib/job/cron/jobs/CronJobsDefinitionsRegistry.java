package com.folib.job.cron.jobs;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.folib.util.ThrowingFunction;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class CronJobsDefinitionsRegistry
{

    private final Set<CronJobDefinition> cronJobDefinitions;

    CronJobsDefinitionsRegistry(final CronJobsRegistry cronJobsRegistry)
    {
        cronJobDefinitions = cronJobsRegistry.get()
                                             .stream()
                                             .map(ThrowingFunction.unchecked(clazz -> clazz.newInstance()
                                                                                           .getCronJobDefinition()))
                                             .collect(Collectors.collectingAndThen(Collectors.toSet(), Collections::unmodifiableSet));
    }

    public Set<CronJobDefinition> getCronJobDefinitions()
    {
        return cronJobDefinitions;
    }

    public Optional<CronJobDefinition> get(String id)
    {
        return cronJobDefinitions.stream()
                                 .filter(d -> StringUtils.equals(d.getJobClass(), id))
                                 .findFirst();
    }
}
