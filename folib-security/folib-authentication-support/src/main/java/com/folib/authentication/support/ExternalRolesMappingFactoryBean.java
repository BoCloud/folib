package com.folib.authentication.support;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.inject.Inject;

import org.springframework.beans.factory.config.AbstractFactoryBean;
import org.springframework.core.env.AbstractEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;

public class ExternalRolesMappingFactoryBean extends AbstractFactoryBean<Map<String, String>>
{

    private Environment env;

    private final String externalAuthenticationId;

    public ExternalRolesMappingFactoryBean(String externalAuthenticationId)
    {
        super();
        this.externalAuthenticationId = externalAuthenticationId;
    }

    public Environment getEnv()
    {
        return env;
    }

    @Inject
    public void setEnv(Environment env)
    {
        this.env = env;
    }

    @Override
    public Class<?> getObjectType()
    {
        return Map.class;
    }

    @Override
    protected Map<String, String> createInstance()
        throws Exception
    {
        Map<String, String> roleMappingMap = new HashMap<>();

        PropertySource<?> propertySource = ((AbstractEnvironment) env).getPropertySources()
                                                                      .get("folib-authentication-providers");
        String[] propertyNames = ((EnumerablePropertySource<?>) propertySource).getPropertyNames();

        Map<String, String> externalRolesMap = new HashMap<>();
        Map<String, String> folibRolesMap = new HashMap<>();

        for (String propertyName : propertyNames)
        {
            String prefix = String.format("folib.authentication.%s.rolesMapping", externalAuthenticationId);
            
            if (!propertyName.startsWith(prefix))
            {
                continue;
            }
            if (propertyName.endsWith("externalRole"))
            {
                externalRolesMap.put(propertyName.replace(prefix, ""),
                                     env.getProperty(propertyName));
            }
            else if (propertyName.endsWith("folibRole"))
            {
                folibRolesMap.put(propertyName.replace(prefix, ""),
                                      env.getProperty(propertyName));
            }
        }

        for (Entry<String, String> externalRoleEntry : externalRolesMap.entrySet())
        {
            String externalRole = externalRoleEntry.getValue();
            String folibRole = folibRolesMap.get(externalRoleEntry.getKey().replace("externalRole",
                                                                                            "folibRole"));

            roleMappingMap.put(externalRole, folibRole);
        }

        return roleMappingMap;
    }

}
