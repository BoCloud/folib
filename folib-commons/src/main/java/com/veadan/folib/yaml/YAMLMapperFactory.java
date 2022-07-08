package com.veadan.folib.yaml;

import javax.annotation.Nonnull;
import java.util.Set;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

/**
 * @author veadan
 */
public interface YAMLMapperFactory
{
    YAMLMapper create(@Nonnull final Set<Class<?>> contextClasses);
}
