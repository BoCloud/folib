package com.folib.job.cron.config;

import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.yaml.YAMLMapperFactory;
import com.folib.yaml.YamlFileManager;

import jakarta.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @author Veadan
 */
@Component
public class CronTasksConfigurationFileManager
        extends YamlFileManager<CronTasksConfigurationDto>
{
    @Value("#{@propertiesPathResolver.resolve('folib.cron.tasks.yaml','etc/conf/folib-cron-tasks.yaml')}")
    private Resource resource;

    @Inject
    public CronTasksConfigurationFileManager(YAMLMapperFactory yamlMapperFactory)
    {
        super(yamlMapperFactory);
    }

    @Override
    protected Resource getResource()
    {
        return resource;
    }
}
