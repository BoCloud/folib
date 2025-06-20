package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.MavenRepositoryConfigurationDto;

import org.springframework.core.convert.converter.Converter;

public enum MavenRepositoryConfigurationConverter
        implements Converter<MavenRepositoryConfigurationDto, com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto convert(MavenRepositoryConfigurationDto mavenRepositoryConfigurationForm)
    {
        com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto configuration = new com.veadan.folib.yaml.configuration.repository.MavenRepositoryConfigurationDto();
        configuration.setIndexingClassNamesEnabled(mavenRepositoryConfigurationForm.isIndexingClassNamesEnabled());
        configuration.setIndexingEnabled(mavenRepositoryConfigurationForm.isIndexingEnabled());
        configuration.setCronExpression(mavenRepositoryConfigurationForm.getCronExpression());
        configuration.setMetadataExpirationStrategy(mavenRepositoryConfigurationForm.getMetadataExpirationStrategy());

        return configuration;
    }
}
