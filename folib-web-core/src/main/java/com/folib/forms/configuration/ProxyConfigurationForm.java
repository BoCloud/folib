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
package com.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.Lists;
import com.folib.configuration.MutableProxyConfiguration;
import com.folib.configuration.ProxyConfiguration;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProxyConfigurationForm
{

    private String host;

    @Min(value = 1, message = "The port number must be an integer between 1 and 65535.", groups = ProxyConfigurationFormChecks.class)
    @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.", groups = ProxyConfigurationFormChecks.class)
    private Integer port;

    @Pattern(regexp = "HTTP|HTTPS",
            flags = Pattern.Flag.CASE_INSENSITIVE,
            message = "The proxy type must contain one the following strings as value: HTTP|HTTPS",
            groups = ProxyConfigurationFormChecks.class)
    private String type;

    private String username;

    private String password;

    private List<String> nonProxyHosts = Lists.newArrayList();

    public ProxyConfigurationForm()
    {
    }

    public ProxyConfigurationForm(String host,
                                  Integer port,
                                  String type,
                                  String username,
                                  String password,
                                  List<String> nonProxyHosts)
    {
        this.host = host;
        this.port = port;
        this.type = type;
        this.username = username;
        this.password = password;
        this.nonProxyHosts = nonProxyHosts;
    }

    @JsonIgnore()
    public static ProxyConfigurationForm fromConfiguration(ProxyConfiguration source)
    {
        ProxyConfiguration configuration = Optional.ofNullable(source).orElse(
                new ProxyConfiguration(new MutableProxyConfiguration())
        );

        return new ProxyConfigurationForm(configuration.getHost(),
                configuration.getPort(),
                configuration.getType(),
                configuration.getUsername(),
                null,
                configuration.getNonProxyHosts());
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

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
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

    public List<String> getNonProxyHosts()
    {
        return nonProxyHosts;
    }

    public void setNonProxyHosts(List<String> nonProxyHosts)
    {
        this.nonProxyHosts = nonProxyHosts;
    }

    @JsonIgnore()
    public MutableProxyConfiguration getMutableProxyConfiguration()
    {
        return new MutableProxyConfiguration(this.host, this.port, this.username, this.password, this.type,
                this.nonProxyHosts);
    }

    public interface ProxyConfigurationFormChecks
            extends Serializable
    {
        // validation group marker interface for fields.
    }

}
