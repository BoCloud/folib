package com.folib.yaml.repository;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
public abstract class CustomRepositoryConfigurationDto implements RepositoryConfiguration, Serializable
{

    @JsonIgnore
    public abstract CustomRepositoryConfiguration getImmutable();

}
