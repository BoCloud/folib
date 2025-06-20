package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.RawRepositoryConfigurationDto;

import org.springframework.core.convert.converter.Converter;

public enum RawRepositoryConfigurationConverter
        implements Converter<RawRepositoryConfigurationDto, com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto convert(RawRepositoryConfigurationDto form)
    {
        return new com.veadan.folib.yaml.configuration.repository.RawRepositoryConfigurationDto();
    }
}
