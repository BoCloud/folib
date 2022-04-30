package com.veadan.folib.yaml.repository;

import javax.annotation.concurrent.Immutable;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "layout")
public abstract class CustomRepositoryConfiguration implements RepositoryConfiguration
{

}
