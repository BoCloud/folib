package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RawRepositoryConfigurationForm;
import com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto;
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
