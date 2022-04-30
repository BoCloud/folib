package com.veadan.folib.configuration;

import com.veadan.folib.yaml.YAMLMapperFactory;
import com.veadan.folib.yaml.YamlFileManager;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import javax.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
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
