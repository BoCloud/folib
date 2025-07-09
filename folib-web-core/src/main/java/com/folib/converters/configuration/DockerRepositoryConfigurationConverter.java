package com.folib.converters.configuration;

import com.folib.configuration.RawRepositoryConfigurationDto;
import com.folib.forms.configuration.RawRepositoryConfigurationForm;
import org.springframework.core.convert.converter.Converter;

public enum DockerRepositoryConfigurationConverter
        implements Converter<RawRepositoryConfigurationForm, RawRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public RawRepositoryConfigurationDto convert(RawRepositoryConfigurationForm form)
    {
        return new RawRepositoryConfigurationDto();
    }
}
