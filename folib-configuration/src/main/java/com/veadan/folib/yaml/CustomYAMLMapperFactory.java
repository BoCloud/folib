package com.veadan.folib.yaml;

import javax.annotation.Nonnull;
import java.util.Set;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class CustomYAMLMapperFactory
        implements YAMLMapperFactory
{

    @Override
    public YAMLMapper create(@Nonnull Set<Class<?>> contextClasses)
    {
        return new StrongboxYamlMapper(contextClasses);
    }
}
