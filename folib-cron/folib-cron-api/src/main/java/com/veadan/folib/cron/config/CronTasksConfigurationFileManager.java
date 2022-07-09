package com.veadan.folib.cron.config;

import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.yaml.YAMLMapperFactory;
import com.veadan.folib.yaml.YamlFileManager;

import javax.inject.Inject;

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
