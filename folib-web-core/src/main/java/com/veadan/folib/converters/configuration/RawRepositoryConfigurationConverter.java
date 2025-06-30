package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RawRepositoryConfigurationForm;

import org.springframework.core.convert.converter.Converter;

public enum RawRepositoryConfigurationConverter
        implements Converter<RawRepositoryConfigurationForm, com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto convert(RawRepositoryConfigurationForm form)
    {
        return new com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto();
    }
}
