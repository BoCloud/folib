package com.folib.yaml;

import javax.annotation.Nonnull;
import java.util.Set;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import com.folib.yaml.FolibYamlMapper;
import com.folib.yaml.YAMLMapperFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class CustomYAMLMapperFactory
        implements YAMLMapperFactory
{

    @Override
    public YAMLMapper create(@Nonnull Set<Class<?>> contextClasses)
    {
        return new FolibYamlMapper(contextClasses);
    }
}
