package com.folib.filter;

import org.springframework.util.StringUtils;

import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;

/**
 * @author veadan
 * @date 2025/4/10
 **/
public class Utf8BasicAuthFilter implements ClientRequestFilter {

    private final String username;
    private final String password;

    public Utf8BasicAuthFilter(String username, String password) {
        this.username = username;
        this.password = password;
    }

    @Override
    public void filter(ClientRequestContext requestContext) throws IOException {
        try {
            if (!StringUtils.hasText(username) || !StringUtils.hasText(password)) {
                return;
            }
            // 生成 UTF-8 编码的 Basic 认证头
            String credentials = username + ":" + password;
            String encodedAuth = Base64.getEncoder()
                    .encodeToString(credentials.getBytes(StandardCharsets.UTF_8));
            String authHeader = "Basic " + encodedAuth;

            // 添加或替换 Authorization 头
            MultivaluedMap<String, Object> headers = requestContext.getHeaders();
            if (Objects.isNull(headers)) {
                headers = new MultivaluedHashMap();
            }
            if (!headers.containsKey(HttpHeaders.AUTHORIZATION)) {
                headers.putSingle(HttpHeaders.AUTHORIZATION, authHeader);
            }
            if (!headers.containsKey(HttpHeaders.USER_AGENT)) {
                headers.putSingle(HttpHeaders.USER_AGENT, "FoLibrary/1.0.0");
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
