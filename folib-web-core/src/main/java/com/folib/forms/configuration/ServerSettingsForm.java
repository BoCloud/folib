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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonSerialize(include = JsonSerialize.Inclusion.ALWAYS)
public class ServerSettingsForm
{

    @NotBlank(message = "The name of this instance")
    @JsonProperty
    private String instanceName;

    @NotBlank(message = "A base URL must be specified.")
    @JsonProperty
    private String baseUrl;

    @NotNull(message = "A port must be specified.")
    @Min(value = 1, message = "The port number must be an integer between 1 and 65535.")
    @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.")
    @JsonProperty
    private Integer port;

    /**
     * 节点传输速率（KB/s）
     */
    @Min(value = 1, message = "The kbps be an integer between 1 and 999999.")
    @Max(value = 999999, message = "The kbps be an integer between 1 and 999999.")
    private Integer kbps;
    /**
     * 节点传输切片大小（MB）
     */
    @Min(value = 1, message = "The sliceMbSize must be an integer between 1 and 10240.")
    @Max(value = 10240, message = "The sliceMbSize must be an integer between 1 and 10240.")
    private Long sliceMbSize;

    @Valid
    @JsonProperty
    private AdvancedConfigurationForm advancedConfigurationForm = new AdvancedConfigurationForm();

    @Valid
    @JsonProperty
    private CorsConfigurationForm corsConfigurationForm = new CorsConfigurationForm();

    @Valid
    @JsonProperty
    private SmtpConfigurationForm smtpConfigurationForm = new SmtpConfigurationForm();

    @Valid
    @JsonProperty
    private ProxyConfigurationForm proxyConfigurationForm = new ProxyConfigurationForm();

    public ServerSettingsForm()
    {
    }

    public ServerSettingsForm(@NotBlank(message = "A base URL must be specified.") String baseUrl,
                              @NotNull(message = "A port must be specified.")
                              @Min(value = 1, message = "The port number must be an integer between 1 and 65535.")
                              @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.") Integer port)
    {
        this.baseUrl = baseUrl;
        this.port = port;
    }

    public ServerSettingsForm(String baseUrl,
                              Integer port,
                              String instanceName,
                              CorsConfigurationForm corsConfigurationForm,
                              SmtpConfigurationForm smtpConfigurationForm,
                              ProxyConfigurationForm proxyConfigurationForm)
    {
        this.baseUrl = baseUrl;
        this.port = port;
        this.instanceName = instanceName;
        this.corsConfigurationForm = corsConfigurationForm;
        this.smtpConfigurationForm = smtpConfigurationForm;
        this.proxyConfigurationForm = proxyConfigurationForm;
    }

    public String getBaseUrl()
    {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl)
    {
        this.baseUrl = baseUrl;
    }

    public Integer getPort()
    {
        return port;
    }

    public void setPort(Integer port)
    {
        this.port = port;
    }

    public Integer getKbps() {
        return kbps;
    }

    public void setKbps(Integer kbps) {
        this.kbps = kbps;
    }

    public Long getSliceMbSize() {
        return sliceMbSize;
    }

    public void setSliceMbSize(Long sliceMbSize) {
        this.sliceMbSize = sliceMbSize;
    }

    public String getInstanceName()
    {
        return instanceName;
    }

    public void setInstanceName(String instanceName)
    {
        this.instanceName = instanceName;
    }

    public CorsConfigurationForm getCorsConfigurationForm()
    {
        return corsConfigurationForm;
    }

    public void setCorsConfigurationForm(CorsConfigurationForm corsConfigurationForm)
    {
        this.corsConfigurationForm = corsConfigurationForm;
    }

    public SmtpConfigurationForm getSmtpConfigurationForm()
    {
        return smtpConfigurationForm;
    }

    public void setSmtpConfigurationForm(SmtpConfigurationForm smtpConfigurationForm)
    {
        this.smtpConfigurationForm = smtpConfigurationForm;
    }

    public ProxyConfigurationForm getProxyConfigurationForm()
    {
        return proxyConfigurationForm;
    }

    public void setProxyConfigurationForm(ProxyConfigurationForm proxyConfigurationForm)
    {
        this.proxyConfigurationForm = proxyConfigurationForm;
    }

    public AdvancedConfigurationForm getAdvancedConfigurationForm() {
        return advancedConfigurationForm;
    }

    public void setAdvancedConfigurationForm(AdvancedConfigurationForm advancedConfigurationForm) {
        this.advancedConfigurationForm = advancedConfigurationForm;
    }
}
