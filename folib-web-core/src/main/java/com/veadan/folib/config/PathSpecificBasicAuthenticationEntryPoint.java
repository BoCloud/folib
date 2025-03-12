package com.veadan.folib.config;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author huayanjun
 * @since 2025-03-12 08:42
 */
public class PathSpecificBasicAuthenticationEntryPoint extends BasicAuthenticationEntryPoint {
    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException {
        String path = request.getRequestURI();
        if (path.startsWith("/dav/")) {
            // 只对 /dav/** 使用自定义逻辑
            response.addHeader("WWW-Authenticate", "Basic realm=\"folib\"");
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authentication Required");
        } else {
            // 其他路径使用默认行为或不处理
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized");
        }
    }

    @Override
    public void afterPropertiesSet() {
        setRealmName("folib");
        super.afterPropertiesSet();
    }
}
