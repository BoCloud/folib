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
package com.folib.storage.search;

import com.folib.artifact.coordinates.ArtifactCoordinates;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author veadan
 */
public class SearchRequest
{

    private String storageId;

    private String repositoryId;

    private String query;

    private Map<String, String> options = new LinkedHashMap<>();

    private ArtifactCoordinates artifactCoordinates;


    public SearchRequest()
    {
    }

    public SearchRequest(String storageId,
                         String repositoryId,
                         String query)
    {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.query = query;
    }

    public SearchRequest(String storageId,
                         String repositoryId,
                         ArtifactCoordinates coordinates)
    {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.artifactCoordinates = coordinates;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public String getQuery()
    {
        return query;
    }

    public void setQuery(String query)
    {
        this.query = query;
    }

    public Map<String, String> getOptions()
    {
        return options;
    }

    public void setOptions(Map<String, String> options)
    {
        this.options = options;
    }

    public String getOption(String key)
    {
        return options.get(key);
    }

    public String addOption(String key,
                            String value)
    {
        return options.put(key, value);
    }

    public boolean removeOption(String key,
                                String value)
    {
        return options.remove(key, value);
    }

    public ArtifactCoordinates getArtifactCoordinates()
    {
        return artifactCoordinates;
    }

    public void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates)
    {
        this.artifactCoordinates = artifactCoordinates;
    }
    
}
