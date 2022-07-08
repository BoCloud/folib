package com.veadan.folib.authorization;

import com.veadan.folib.yaml.YAMLMapperFactory;
import com.veadan.folib.yaml.YamlFileManager;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;

import javax.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @author Pablo Tirado
 */
@Component
public class AuthorizationConfigFileManager
        extends YamlFileManager<AuthorizationConfigDto>
{
    @Value("#{@propertiesPathResolver.resolve('folib.authorization.config.yaml','etc/conf/folib-authorization.yaml')}")
    private Resource resource;

    @Inject
    public AuthorizationConfigFileManager(YAMLMapperFactory yamlMapperFactory)
    {
        super(yamlMapperFactory);
    }

    @Override
    protected Resource getResource()
    {
        return resource;
    }
}
