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
import com.folib.configuration.MutableWebhookConfiguration;
import com.folib.configuration.WebhookConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Optional;
import java.util.Set;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WebhookConfigurationForm
        implements Serializable {

    /**
     * uuid
     */
    @NotBlank(message = "uuid不能为空", groups = {UpdateGroup.class, DeleteGroup.class, TestGroup.class})
    private String uuid;
    /**
     * url
     */
    @NotBlank(message = "请填写url", groups = {AddGroup.class})
    private String url;

    /**
     * 访问令牌
     */
    private String accessToken;

    /**
     * 触发事件
     */
    @NotEmpty(message = "请选择触发事件", groups = {AddGroup.class, TestGroup.class})
    private Set<String> events;
    /**
     * 启用ssl true 启用 false 不启用
     */
    private Boolean ssl;

    /**
     * 所属仓库 不填则对所有仓库生效
     */
    private Set<String>  repository;

    public WebhookConfigurationForm(WebhookConfiguration webhookConfiguration) {
        this.uuid = webhookConfiguration.getUuid();
        this.url = webhookConfiguration.getUrl();
        this.accessToken = webhookConfiguration.getAccessToken();
        this.events = webhookConfiguration.getEvents();
        this.ssl = webhookConfiguration.getSsl();
        this.repository= webhookConfiguration.getRepository();
    }

    @JsonIgnore()
    public static WebhookConfigurationForm fromConfiguration(WebhookConfiguration source) {
        WebhookConfiguration webhookConfiguration = Optional.ofNullable(source).orElse(
                new WebhookConfiguration(new MutableWebhookConfiguration())
        );
        return new WebhookConfigurationForm(webhookConfiguration);
    }

    public interface AddGroup
            extends Serializable {
    }

    public interface UpdateGroup
            extends Serializable {
    }

    public interface DeleteGroup
            extends Serializable {
    }

    public interface TestGroup
            extends Serializable {
    }
}