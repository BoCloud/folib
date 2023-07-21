package com.veadan.folib.authorization.domain;

import com.veadan.folib.authorization.dto.RoleDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;

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
     * 客户端名称
     */
    private String clientName;

    /**
     * 重定向地址
     */
    private String  redirectPath;

    /**
     * sso登录地址
     */
    private String ssoPath;

    /**
     * 秒速信息
     */
    private String desc;

    /**
     * 退出单点登录的uri
     * @param source
     */
    private String loginOutUrl;

    /**
     * 退出单点登录的重定向的url
     * @param source
     */
    private String loginOutRedPath;

    /**
     * 获取accesstoken的url
     * @param source
     */
    private String  access_token_url;


    /**
     * 退出单点登录的重定向的url
     * @param source
     */
    public Client(final Client source)
    {
        this.clientId =source.getClientId();
        this.redirectPath=source.getRedirectPath();
        this.ssoPath=source.getSsoPath();
        this.desc=source.getDesc();
        this.loginOutUrl=source.getLoginOutUrl();
        this.loginOutRedPath=source.getLoginOutRedPath();
        this.clientName=source.getClientName();
        this.access_token_url=source.getAccess_token_url();
    }
}
