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
import com.folib.configuration.MutableSmtpConfiguration;
import com.folib.configuration.SmtpConfiguration;

import javax.validation.constraints.*;
import java.io.Serializable;
import java.util.Optional;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SmtpConfigurationForm {

    @NotBlank(message = "An SMTP host must be provided.", groups = SmtpConfigurationFormChecks.class)
    private String host;

    @NotNull(message = "The SMTP port must be provided.", groups = SmtpConfigurationFormChecks.class)
    @Min(value = 1, message = "The port number must be an integer between 1 and 65535.", groups = SmtpConfigurationFormChecks.class)
    @Max(value = 65535, message = "The port number must be an integer between 1 and 65535.", groups = SmtpConfigurationFormChecks.class)
    private Integer port;

    private String username;

    private String password;

    @Pattern(regexp = "plain|ssl|tls",
            flags = Pattern.Flag.CASE_INSENSITIVE,
            message = "Please, set a valid SMTP connection type.",
            groups = SmtpConfigurationFormChecks.class)
    private String connection;

    public SmtpConfigurationForm() {
    }

    public SmtpConfigurationForm(String host,
                                 Integer port,
                                 String connection,
                                 String username,
                                 String password) {
        this.host = host;
        this.port = port;
        this.connection = connection;
        this.username = username;
        this.password = password;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public Integer getPort() {
        return port;
    }

    public void setPort(Integer port) {
        this.port = port;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getConnection() {
        return connection;
    }

    public void setConnection(String connection) {
        this.connection = connection;
    }

    @JsonIgnore()
    public MutableSmtpConfiguration getMutableSmtpConfiguration() {
        return new MutableSmtpConfiguration(this.host,
                this.port,
                this.connection,
                this.username,
                this.password);
    }

    @JsonIgnore()
    public static SmtpConfigurationForm fromConfiguration(SmtpConfiguration source) {
        SmtpConfiguration configuration = Optional.ofNullable(source).orElse(
                new SmtpConfiguration(new MutableSmtpConfiguration())
        );

        return new SmtpConfigurationForm(configuration.getHost(),
                configuration.getPort(),
                configuration.getConnection(),
                configuration.getUsername(),
                configuration.getPassword());
    }

    public interface SmtpConfigurationFormChecks
            extends Serializable {
        // validation group marker interface for fields.
    }

}
