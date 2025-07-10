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

import java.util.LinkedHashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * @author veadan
 */
@JsonRootName("artifacts")
public class SearchResults
{

    @JsonProperty("artifact")
    private Set<SearchResult> results = new LinkedHashSet<>();

    private long total;

    public long getTotal() {
        return total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    public SearchResults()
    {
    }

    public Set<SearchResult> getResults()
    {
        return results;
    }

    public void setResults(Set<SearchResult> results)
    {
        this.results = results;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();

        for (SearchResult artifact : results)
        {
            sb.append(artifact.getStorageId()).append(':').append(artifact.getRepositoryId()).append(' ');
            sb.append(artifact.getPath()).append(':');
            sb.append(' ');
            sb.append(artifact.getUrl());
            sb.append('\n');
        }

        return sb.toString();
    }

}
