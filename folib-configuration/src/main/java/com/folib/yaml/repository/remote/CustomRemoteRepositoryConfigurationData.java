package com.folib.yaml.repository.remote;

import javax.annotation.concurrent.Immutable;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

@Immutable
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "layout")
public abstract class CustomRemoteRepositoryConfigurationData
        implements CustomRemoteRepositoryConfiguration
{

}
