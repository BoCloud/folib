package com.folib.yaml.configuration.repository;

import com.folib.yaml.repository.RepositoryConfiguration;

public interface NugetRepositoryConfiguration extends RepositoryConfiguration
{

    String getFeedVersion();

    Integer getRemoteFeedPageSize();

}
