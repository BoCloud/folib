package com.folib.mapper;

import com.folib.forms.configuration.CustomRepositoryConfigurationForm;
import com.folib.yaml.ObjectMapperSubtypes;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class WebObjectMapperSubtypes
        extends ObjectMapperSubtypes
{

    public static final WebObjectMapperSubtypes INSTANCE = new WebObjectMapperSubtypes();

    private static final Set<Class<?>> ADDITIONAL_TYPES = ImmutableSet.of(CustomRepositoryConfigurationForm.class);

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
