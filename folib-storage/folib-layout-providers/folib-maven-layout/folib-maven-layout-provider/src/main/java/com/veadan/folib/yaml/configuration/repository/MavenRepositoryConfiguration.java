package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.yaml.repository.RepositoryConfiguration;

public interface MavenRepositoryConfiguration extends RepositoryConfiguration
{

    boolean isIndexingEnabled();

    boolean isIndexingClassNamesEnabled();

    String getCronExpression();

    String getMetadataExpirationStrategy();

}
