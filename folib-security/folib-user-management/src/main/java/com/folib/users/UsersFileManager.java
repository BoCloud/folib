package com.folib.users;

import com.folib.users.dto.UsersDto;
import com.folib.yaml.YAMLMapperFactory;
import com.folib.yaml.YamlFileManager;

import javax.inject.Inject;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @author Veadan
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
