package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author veadan
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "layout")
public abstract class CustomRepositoryConfigurationDto
{

    public abstract <T> T accept(CustomRepositoryConfigurationDtoVisitor<T> visitor);
}
