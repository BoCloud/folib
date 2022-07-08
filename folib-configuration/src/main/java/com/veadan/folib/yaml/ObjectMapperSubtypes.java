package com.veadan.folib.yaml;

import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;
import com.veadan.folib.util.ServiceLoaderUtils;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class ObjectMapperSubtypes
{

    public static final ObjectMapperSubtypes INSTANCE = new ObjectMapperSubtypes();

    private static final Set<Class<?>> TYPES = ImmutableSet.of(CustomRepositoryConfiguration.class,
                                                               CustomRepositoryConfigurationDto.class,
                                                               RemoteRepositoryConfigurationDto.class);

    private volatile Set<Class<?>> subtypes;

    protected ObjectMapperSubtypes()
    {

    }

    public final synchronized Set<Class<?>> subtypes()
    {
        if (subtypes == null)
        {
            subtypes = ServiceLoaderUtils.load(getTypes().toArray(new Class<?>[0]));
        }
        return subtypes;
    }

    protected Set<Class<?>> getTypes()
    {
        return TYPES;
    }


}
