package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.NugetRepositoryConfigurationDto;

import org.springframework.core.convert.converter.Converter;

public enum NugetRepositoryConfigurationConverter
        implements Converter<NugetRepositoryConfigurationDto, com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto convert(NugetRepositoryConfigurationDto form)
    {
        com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto configuration = new com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto();
        configuration.setFeedVersion(form.getFeedVersion());
        configuration.setRemoteFeedPageSize(form.getRemoteFeedPageSize());

        return configuration;
    }
}
