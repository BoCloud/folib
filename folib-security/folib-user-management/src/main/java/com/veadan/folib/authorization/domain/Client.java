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


    public Client(final Client source)
    {
        this.clientId =source.getClientId();
        this.redirectPath=source.getRedirectPath();
        this.ssoPath=source.getSsoPath();
        this.desc=source.getDesc();
    }
}
