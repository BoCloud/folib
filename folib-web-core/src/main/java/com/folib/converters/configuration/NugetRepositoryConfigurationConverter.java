package com.folib.converters.configuration;

import com.folib.yaml.configuration.repository.NugetRepositoryConfigurationDto;
import com.folib.forms.configuration.NugetRepositoryConfigurationForm;

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
