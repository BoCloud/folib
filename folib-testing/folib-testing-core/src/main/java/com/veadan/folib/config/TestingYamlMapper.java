package com.veadan.folib.config;

import com.veadan.folib.yaml.FolibYamlMapper;

import javax.annotation.Nonnull;
import java.util.Set;

import com.fasterxml.jackson.databind.DeserializationFeature;

/**
 * @author Przemyslaw Fusik
 */
public class TestingYamlMapper
        extends FolibYamlMapper
{

    public TestingYamlMapper(@Nonnull final Set<Class<?>> contextClasses)
    {
        super(contextClasses);
        disable(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE);
    }
}
