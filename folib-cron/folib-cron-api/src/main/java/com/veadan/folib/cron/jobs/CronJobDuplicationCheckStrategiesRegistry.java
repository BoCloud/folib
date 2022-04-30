package com.veadan.folib.cron.jobs;

import com.veadan.folib.util.ThrowingFunction;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class CronJobDuplicationCheckStrategiesRegistry
{

    private final Map<String, Set<CronJobDuplicationCheckStrategy>> duplicationStrategies;

    CronJobDuplicationCheckStrategiesRegistry(final CronJobsRegistry cronJobsRegistry)
    {
        duplicationStrategies = cronJobsRegistry.get()
                                                .stream()
                                                .collect(Collectors.toMap(Class::getName,
                                                                          ThrowingFunction.unchecked(
                                                                                  clazz -> clazz.getDeclaredConstructor()
                                                                                                .newInstance()
                                                                                                .getDuplicationStrategies())));
    }

    public Set<CronJobDuplicationCheckStrategy> get(String jobClass)
    {
        return duplicationStrategies.get(jobClass);
    }

}
