package com.veadan.folib.storage.repository;

import com.veadan.folib.yaml.CustomTag;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
public abstract class MutableCustomConfiguration
        implements CustomTag
{

    @JsonIgnore
    public abstract CustomConfiguration getImmutable();
}
