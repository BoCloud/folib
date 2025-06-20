package com.veadan.folib.converters.cron;

import com.veadan.folib.dto.cron.CronTaskConfigurationDto;
import com.veadan.folib.dto.cron.CronTaskConfigurationDtoField;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public enum CronTaskConfigurationFormToCronTaskConfigurationDtoConverter
        implements Converter<CronTaskConfigurationDto, com.veadan.folib.job.cron.domain.CronTaskConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.job.cron.domain.CronTaskConfigurationDto convert(CronTaskConfigurationDto configurationForm)
    {
        com.veadan.folib.job.cron.domain.CronTaskConfigurationDto configuration = new com.veadan.folib.job.cron.domain.CronTaskConfigurationDto();

        List<CronTaskConfigurationDtoField> fields = configurationForm.getFields();
        if (CollectionUtils.isNotEmpty(fields))
        {
            configuration.setProperties(fields.stream()
                                              .collect(Collectors.toMap(f -> f.getName(), f -> f.getValue())));
        }

        configuration.setJobClass(configurationForm.getJobClass());
        configuration.setCronExpression(configurationForm.getCronExpression());
        configuration.setOneTimeExecution(configurationForm.isOneTimeExecution());
        configuration.setImmediateExecution(configurationForm.isImmediateExecution());
        return configuration;
    }
}
