package com.veadan.folib.cron.domain;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * @author veadan
 * @author Veadan
 */
@JsonRootName("cronTasksConfiguration")
public class CronTasksConfigurationDto
        implements Serializable
{

    private Set<CronTaskConfigurationDto> cronTaskConfigurations;

    public Set<CronTaskConfigurationDto> getCronTaskConfigurations()
    {
        return cronTaskConfigurations != null ? cronTaskConfigurations : (cronTaskConfigurations = new LinkedHashSet<>());
    }

    public void setCronTaskConfigurations(Set<CronTaskConfigurationDto> cronTaskConfigurations)
    {
        this.cronTaskConfigurations = cronTaskConfigurations;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("CronTasksConfiguration{");
        sb.append("cronTaskConfigurations=").append(cronTaskConfigurations);
        sb.append('}');
        return sb.toString();
    }
}
