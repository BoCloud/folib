package com.veadan.folib.authorization.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;

/**
 * @author leipenghui
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
