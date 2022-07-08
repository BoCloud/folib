package com.veadan.folib.users;

import com.veadan.folib.users.dto.UsersDto;
import com.veadan.folib.yaml.YAMLMapperFactory;
import com.veadan.folib.yaml.YamlFileManager;

import javax.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @author Pablo Tirado
 */
@Component
public class UsersFileManager
        extends YamlFileManager<UsersDto>
{
    @Value("#{@propertiesPathResolver.resolve('folib.users.config.yaml','etc/conf/folib-security-users.yaml')}")
    private Resource resource;

    @Inject
    public UsersFileManager(YAMLMapperFactory yamlMapperFactory)
    {
        super(yamlMapperFactory);
    }

    @Override
    protected Resource getResource()
    {
        return resource;
    }
}
