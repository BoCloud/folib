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
package com.folib.components.thirdparty.foeyes;

import com.folib.components.DistributedCacheComponent;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author veadan
 * @date 2024/4/25
 **/
@Component
public class FoEyesProperties {

    @Value("${thirdParty.foeyes.enable}")
    private boolean enable;

    @Value("${thirdParty.foeyes.baseUrl}")
    private String baseUrl;

    @Value("${thirdParty.foeyes.accessKey}")
    private String accessKey;

    @Value("${thirdParty.foeyes.username}")
    private String username;

    @Value("${thirdParty.foeyes.password}")
    private String password;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    public boolean getEnable() {
        String value = getCache("enable");
        if (StringUtils.isNotBlank(value)) {
            return Boolean.parseBoolean(value);
        }
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    public String getBaseUrl() {
        String value = getCache("baseUrl");
        if (StringUtils.isNotBlank(value)) {
            return value;
        }
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    public String getAccessKey() {
        String value = getCache("accessKey");
        if (StringUtils.isNotBlank(value)) {
            return value;
        }
        return accessKey;
    }

    public void setAccessKey(String accessKey) {
        this.accessKey = accessKey;
    }

    public String getUsername() {
        String value = getCache("username");
        if (StringUtils.isNotBlank(value)) {
            return value;
        }
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        String value = getCache("password");
        if (StringUtils.isNotBlank(value)) {
            return value;
        }
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    private String getCache(String name) {
        String key = String.format("THIRD_PARTY_FO_EYES_%s_KEY", name.toUpperCase());
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(value)) {
            return null;
        }
        return value;
    }
}
