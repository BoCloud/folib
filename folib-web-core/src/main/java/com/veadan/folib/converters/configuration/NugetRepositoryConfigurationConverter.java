package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.NugetRepositoryConfigurationForm;
import com.veadan.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto;

import org.springframework.core.convert.converter.Converter;

public enum NugetRepositoryConfigurationConverter
        implements Converter<NugetRepositoryConfigurationForm, NugetRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public NugetRepositoryConfigurationDto convert(NugetRepositoryConfigurationForm form)
    {
        NugetRepositoryConfigurationDto configuration = new NugetRepositoryConfigurationDto();
        configuration.setFeedVersion(form.getFeedVersion());
        configuration.setRemoteFeedPageSize(form.getRemoteFeedPageSize());

        return configuration;
    }
}
