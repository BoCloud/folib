package com.veadan.folib.components.security;

import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jose4j.jwt.JwtClaims;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Invocation;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class SecurityComponent {

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    private String securityToken;

    public String getSecurityToken() {
        if (StringUtils.isNotBlank(securityToken)) {
            JwtClaims jwtClaims = securityTokenProvider.getClaims(securityToken, false);
            try {
                long expirationTime = jwtClaims.getExpirationTime().getValueInMillis();
                long currentTime = System.currentTimeMillis();
                long interval = 600 * 1000;
                long differTime = expirationTime - currentTime;
                if (differTime <= interval) {
                    log.info("授权token的过期时间还有：{} 毫秒，重新生成", differTime);
                } else {
                    log.info("授权token的过期时间还有：{} 毫秒，继续使用", differTime);
                    return securityToken;
                }
            } catch (Exception ex) {
                log.error("获取授权token的过期时间错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        generateSecurityToken();
        log.info("当前的授权token：{} ", securityToken);
        return securityToken;
    }

    private void generateSecurityToken() {
        String admin = "admin";
        try {
            securityToken = userService.generateSecurityToken(admin);
            log.info("已生成授权token：{}", securityToken);
        } catch (Exception ex) {
            log.error("生成授权token错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    public Invocation.Builder securityTokenHeader(Invocation.Builder builder) {
        builder.header(JwtTokenFetcher.AUTHORIZATION_HEADER, JwtTokenFetcher.BEARER_AUTHORIZATION_PREFIX + " " + getSecurityToken());
        return builder;
    }

}