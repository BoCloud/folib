package com.folib.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;

/**
 * @author veadan
 * @since 2024-12-31 16:45
 */
@Component
public class SecurityUtils {

    @Autowired
    private UserDetailsService userDetailsService;

    public void setAdminAuthentication() {
        // 创建一个带有管理员权限的 Authentication

        UserDetails admin = userDetailsService.loadUserByUsername("admin");
        UsernamePasswordAuthenticationToken adminAuth = new UsernamePasswordAuthenticationToken(
                admin, // 用户名
                null,    // 密码 (可为 null)
                Collections.singletonList(new SimpleGrantedAuthority("ADMIN")) // 管理员权限
        );

        // 设置到 SecurityContext
        SecurityContextHolder.getContext().setAuthentication(adminAuth);

    }

    public void clearAuthentication() {
        SecurityContextHolder.clearContext();
    }
}
