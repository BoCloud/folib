package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author veadan
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "layout")
public abstract class CustomRepositoryConfigurationForm
{

    public abstract <T> T accept(CustomRepositoryConfigurationFormVisitor<T> visitor);
}
