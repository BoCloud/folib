package com.veadan.folib.mapper;

import com.veadan.folib.dto.configuration.CustomRepositoryConfigurationDto;
import com.veadan.folib.yaml.ObjectMapperSubtypes;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class WebObjectMapperSubtypes
        extends ObjectMapperSubtypes
{

    public static final WebObjectMapperSubtypes INSTANCE = new WebObjectMapperSubtypes();

    private static final Set<Class<?>> ADDITIONAL_TYPES = ImmutableSet.of(CustomRepositoryConfigurationDto.class);

    private WebObjectMapperSubtypes()
    {

    }

    @Override
    protected Set<Class<?>> getTypes()
    {
        ImmutableSet.Builder<Class<?>> builder = ImmutableSet.builder();
        builder.addAll(super.getTypes());
        builder.addAll(ADDITIONAL_TYPES);
        return builder.build();
    }
}
