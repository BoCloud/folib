package com.folib.authorization;

import com.folib.yaml.YAMLMapperFactory;
import com.folib.yaml.YamlFileManager;
import com.folib.authorization.dto.AuthorizationConfigDto;

import javax.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @author Veadan
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
