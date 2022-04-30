package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.MavenRepositoryConfigurationForm;
import com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto;

import org.springframework.core.convert.converter.Converter;

public enum MavenRepositoryConfigurationConverter
        implements Converter<MavenRepositoryConfigurationForm, MavenRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public MavenRepositoryConfigurationDto convert(MavenRepositoryConfigurationForm mavenRepositoryConfigurationForm)
    {
        MavenRepositoryConfigurationDto configuration = new MavenRepositoryConfigurationDto();
        configuration.setIndexingClassNamesEnabled(mavenRepositoryConfigurationForm.isIndexingClassNamesEnabled());
        configuration.setIndexingEnabled(mavenRepositoryConfigurationForm.isIndexingEnabled());
        configuration.setCronExpression(mavenRepositoryConfigurationForm.getCronExpression());
        configuration.setMetadataExpirationStrategy(mavenRepositoryConfigurationForm.getMetadataExpirationStrategy());

        return configuration;
    }
}
