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
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableProxyConfiguration
        implements Serializable
{

    private String host;

    private Integer port;

    private String username;

    private String password;

    /**
     * Proxy type (HTTP, SOCKS5, etc)
     */
    private String type;

    private List<String> nonProxyHosts = new ArrayList<>();

    @JsonCreator
    public MutableProxyConfiguration()
    {
    }

    @JsonCreator
    public MutableProxyConfiguration(@JsonProperty("host") String host,
                                     @JsonProperty("port") Integer port,
                                     @JsonProperty("username") String username,
                                     @JsonProperty("password") String password,
                                     @JsonProperty("type") String type,
                                     @JsonProperty("nonProxyHosts") List<String> nonProxyHosts)
    {
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.type = type;
        this.nonProxyHosts = nonProxyHosts;
    }
    public String getHost()
    {
        return host;
    }

    public void setHost(String host)
    {
        this.host = host;
    }

    public Integer getPort()
    {
        return port;
    }

    public void setPort(Integer port)
    {
        this.port = port;
    }

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

    public List<String> getNonProxyHosts()
    {
        return nonProxyHosts;
    }

    public void setNonProxyHosts(List<String> nonProxyHosts)
    {
        this.nonProxyHosts = nonProxyHosts;
    }

    public void addNonProxyHost(String host)
    {
        nonProxyHosts.add(host);
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MutableProxyConfiguration that = (MutableProxyConfiguration) o;
        return Objects.equal(port, that.port) &&
               Objects.equal(host, that.host) &&
               Objects.equal(username, that.username) &&
               Objects.equal(password, that.password) &&
               Objects.equal(type, that.type) &&
               Objects.equal(nonProxyHosts, that.nonProxyHosts);
    }

    @Override
    public int hashCode()
    {
        return Objects.hashCode(host, port, username, password, type, nonProxyHosts);
    }

    @Override
    public String toString()
    {
        return MoreObjects.toStringHelper(this)
                          .add("host", host)
                          .add("port", port)
                          .add("username", username)
                          .add("password", password)
                          .add("type", type)
                          .add("nonProxyHosts", nonProxyHosts)
                          .toString();
    }

}
