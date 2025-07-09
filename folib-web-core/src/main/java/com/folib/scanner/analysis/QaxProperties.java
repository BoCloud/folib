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
package com.folib.scanner.analysis;

import cn.hutool.core.util.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class QaxProperties {

    @Value("${thirdParty.qax.enable}")
    private Boolean enable;
    @Value("${thirdParty.qax.baseUrl}")
    private String baseUrl;
    @Value("${thirdParty.qax.privateToken}")
    private String privateToken;
    @Value("${thirdParty.qax.projectId}")
    private String projectId;
    @Value("${thirdParty.qax.username}")
    private String username;
    @Value("${thirdParty.qax.password}")
    private String password;

    @Value("${thirdParty.qax.tags}")
    private String tags;

    public String getBaseUrl() {
        return  StrUtil.trimToNull(baseUrl);
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = StrUtil.trimToNull(baseUrl);
    }

    public String getPrivateToken() {
        return  StrUtil.trimToNull(privateToken);
    }

    public void setPrivateToken(String privateToken) {
        this.privateToken = StrUtil.trimToNull(privateToken);
    }

    public String getProjectId() {
        return  StrUtil.trimToNull(projectId);
    }

    public void setProjectId(String projectId) {
        this.projectId = StrUtil.trimToNull(projectId);
    }

    public String getUsername() {
        return  StrUtil.trimToNull(username);
    }

    public String getPassword() {
        return  StrUtil.trimToNull(password);
    }

    public void setUsername(String username) {
        this.username = StrUtil.trimToNull(username);
    }

    public void setPassword(String password) {
        this.password = StrUtil.trimToNull(password);
    }

    public boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public String getTags() {
        return  StrUtil.trimToNull(tags);
    }

    public void setTags(String tags) {
        this.tags = StrUtil.trimToNull(tags);
    }

}
