package com.folib.yaml.configuration.repository;

import com.folib.yaml.repository.RepositoryConfiguration;

public interface MavenRepositoryConfiguration extends RepositoryConfiguration
{

    boolean isIndexingEnabled();

    boolean isIndexingClassNamesEnabled();

    String getCronExpression();

    String getMetadataExpirationStrategy();

}
