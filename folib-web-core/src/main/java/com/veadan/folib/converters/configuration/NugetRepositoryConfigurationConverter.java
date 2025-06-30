package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.NugetRepositoryConfigurationForm;

import org.springframework.core.convert.converter.Converter;

public enum NugetRepositoryConfigurationConverter
        implements Converter<NugetRepositoryConfigurationForm, com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto convert(NugetRepositoryConfigurationForm form)
    {
        com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto configuration = new com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto();
        configuration.setFeedVersion(form.getFeedVersion());
        configuration.setRemoteFeedPageSize(form.getRemoteFeedPageSize());

        return configuration;
    }
}
