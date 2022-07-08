package com.veadan.folib.cron.domain;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class CronTasksConfiguration
{

    private final Set<CronTaskConfiguration> cronTaskConfigurations;

    public CronTasksConfiguration(final CronTasksConfigurationDto source)
    {
        this.cronTaskConfigurations = immuteCronTaskConfigurations(source.getCronTaskConfigurations());
    }

    private Set<CronTaskConfiguration> immuteCronTaskConfigurations(final Set<CronTaskConfigurationDto> source)
    {
        return source != null ? ImmutableSet.copyOf(
                source.stream().map(CronTaskConfiguration::new).collect(Collectors.toList())) :
               Collections.emptySet();
    }
}
