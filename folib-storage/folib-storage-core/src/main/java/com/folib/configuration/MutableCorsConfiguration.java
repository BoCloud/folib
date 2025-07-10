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
package com.folib.configuration;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableCorsConfiguration
        implements Serializable
{

    private List<String> allowedOrigins;

    private List<String> allowedMethods;

    private List<String> allowedHeaders;

    private List<String> exposedHeaders;

    @JsonProperty("allowedCredentials")
    private Boolean allowCredentials;

    private Long maxAge;

    @JsonCreator
    public MutableCorsConfiguration()
    {
    }

    @JsonCreator
    public MutableCorsConfiguration(@JsonProperty("allowedOrigins") List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public List<String> getAllowedOrigins()
    {
        return allowedOrigins;
    }

    public void setAllowedOrigins(final List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public List<String> getAllowedMethods()
    {
        return allowedMethods;
    }

    public void setAllowedMethods(final List<String> allowedMethods)
    {
        this.allowedMethods = allowedMethods;
    }

    public List<String> getAllowedHeaders()
    {
        return allowedHeaders;
    }

    public void setAllowedHeaders(final List<String> allowedHeaders)
    {
        this.allowedHeaders = allowedHeaders;
    }

    public List<String> getExposedHeaders()
    {
        return exposedHeaders;
    }

    public void setExposedHeaders(final List<String> exposedHeaders)
    {
        this.exposedHeaders = exposedHeaders;
    }

    public Boolean getAllowCredentials()
    {
        return allowCredentials;
    }

    public void setAllowCredentials(final Boolean allowCredentials)
    {
        this.allowCredentials = allowCredentials;
    }

    public Long getMaxAge()
    {
        return maxAge;
    }

    public void setMaxAge(final Long maxAge)
    {
        this.maxAge = maxAge;
    }
}
