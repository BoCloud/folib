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
package com.folib.authorization.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;

/**
 * @author veadan
 */
@Immutable
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Client implements Serializable {

    /**
     * 客户端id
     */
    private String clientId;

    /**
     * 客户端密钥
     */
    private String clientSecret;

    /**
     * 客户端名称
     */
    private String clientName;

    /**
     * 重定向地址
     */
    private String redirectPath;

    /**
     * sso登录地址
     */
    private String ssoPath;

    /**
     * 用户信息的接口地址
     */
    private String userInfoUrl;

    /**
     * username
     */
    private String username;

    /**
     * 秒速信息
     */
    private String desc;

    /**
     * 退出单点登录的uri
     */
    private String loginOutUrl;

    /**
     * 退出单点登录的重定向的url
     */
    private String loginOutRedPath;

    /**
     * 获取accessToken的url
     */
    private String accessTokenUrl;

    /**
     * 退出单点登录的重定向的url
     */
    public Client(final Client source) {
        this.clientId = source.getClientId();
        this.redirectPath = source.getRedirectPath();
        this.ssoPath = source.getSsoPath();
        this.desc = source.getDesc();
        this.loginOutUrl = source.getLoginOutUrl();
        this.loginOutRedPath = source.getLoginOutRedPath();
        this.clientName = source.getClientName();
        this.accessTokenUrl = source.getAccessTokenUrl();
    }
}
