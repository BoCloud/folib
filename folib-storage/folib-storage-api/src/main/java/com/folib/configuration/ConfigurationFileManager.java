package com.folib.configuration;

import com.folib.yaml.YAMLMapperFactory;
import com.folib.yaml.YamlFileManager;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import jakarta.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ConfigurationFileManager
        extends YamlFileManager<MutableConfiguration>
{
    @Value("#{@propertiesPathResolver.resolve('folib.config.file','etc/conf/folib.yaml')}")
    private Resource resource;

    @Inject
    public ConfigurationFileManager(YAMLMapperFactory yamlMapperFactory)
    {
        super(yamlMapperFactory, CustomRepositoryConfigurationDto.class, RemoteRepositoryConfigurationDto.class);
    }

    @Override
    protected Resource getResource()
    {
        return resource;
    }
}
