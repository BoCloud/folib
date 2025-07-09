/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.metadata;

/**
 * @author veadan
 */
public enum MetadataType
{

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:bar:2.0" --> "org/foo/bar/maven-metadata.xml).
     */
    ARTIFACT_ROOT_LEVEL("artifact"),

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:bar:2.0-SNAPSHOT" --> "org/foo/bar/2.0/maven-metadata.xml).
     */
    SNAPSHOT_VERSION_LEVEL("snapshot"),

    /**
     * Used for artifact root-level metadata.
     *
     * (For example, "org.foo:some-maven-plugin:1.2.3" --> "org/foo/maven-metadata.xml).
     */
    PLUGIN_GROUP_LEVEL("plugin");

    private String type;


    MetadataType(String type)
    {
        this.type = type;
    }

    public static MetadataType from(String metadataType)
            throws IllegalArgumentException, UnsupportedOperationException
    {
        if (metadataType == null)
        {
            throw new IllegalArgumentException("Invalid metadata type!");
        }

        if (MetadataType.ARTIFACT_ROOT_LEVEL.getType().equals(metadataType))
        {
            return ARTIFACT_ROOT_LEVEL;
        }
        else if (MetadataType.SNAPSHOT_VERSION_LEVEL.getType().equals(metadataType))
        {
            return SNAPSHOT_VERSION_LEVEL;
        }
        else if (MetadataType.PLUGIN_GROUP_LEVEL.getType().equals(metadataType))
        {
            return PLUGIN_GROUP_LEVEL;
        }

        throw new UnsupportedOperationException("Unsupported metadata type!");
    }

    public String getType()
    {
        return type;
    }

}
