package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.yaml.repository.RepositoryConfiguration;

public interface NugetRepositoryConfiguration extends RepositoryConfiguration
{

    String getFeedVersion();

    Integer getRemoteFeedPageSize();

}
