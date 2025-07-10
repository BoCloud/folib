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
package com.folib.config;

import com.folib.authentication.AuthenticationConfig;
import com.folib.authorization.dto.Role;
import com.folib.configuration.ConfigurationManager;
import com.folib.security.CustomAccessDeniedHandler;
import com.folib.security.authentication.FolibAuthenticationFilter;
import com.folib.security.authentication.Http401AuthenticationEntryPoint;
import com.folib.security.authentication.suppliers.AuthenticationSupplier;
import com.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.folib.security.vote.ExtendedAuthoritiesVoter;
import com.folib.security.vote.ExtendedAuthorizationManager;

import com.folib.services.ConfigurationManagementService;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.AuthoritiesProvider;
import org.apache.commons.lang.BooleanUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.actuate.autoconfigure.security.servlet.EndpointRequest;
import org.springframework.context.annotation.*;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.authorization.method.AuthorizationManagerBeforeMethodInterceptor;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.firewall.HttpFirewall;
import org.springframework.security.web.firewall.StrictHttpFirewall;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import javax.inject.Inject;
import javax.inject.Qualifier;
import java.util.ArrayList;
import java.util.List;

import static org.springframework.security.config.Customizer.withDefaults;


@Configuration
@ComponentScan({"com.folib.security"})
@Import({DataServiceConfig.class,
        UsersConfig.class,
        AuthenticationConfig.class})
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class WebSecurityConfig {

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private AuthenticationManager authenticationManager;

    @Inject
    private List<AuthenticationSupplier> suppliers;

    @Inject
    private Http401AuthenticationEntryPoint customEntryPoint;

    @Inject
    @Lazy
    private ConfigurationManager configurationManager;
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http .addFilterBefore(folibAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
                .exceptionHandling(exception -> exception
                        .accessDeniedHandler(accessDeniedHandler())
                        .authenticationEntryPoint(customBasicAuthenticationEntryPoint())
                )
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(EndpointRequest.toAnyEndpoint()).hasAuthority("ADMIN")
                        .requestMatchers("/dav/**").authenticated()
                        .requestMatchers("/favicon.ico", "/ui/**", "/docs/**", "/webjars/**", "/rest/**").permitAll()
                        .anyRequest().permitAll())
                .anonymous(anon -> anon
                        .authenticationFilter(anonymousAuthenticationFilter()))
                .cors(withDefaults())
                .csrf(AbstractHttpConfigurer::disable);

        return http.build();
    }

    // 替换 WebSecurity 自定义配置
    @Bean
    public HttpFirewall allowUrlEncodedSlashHttpFirewall() {
        StrictHttpFirewall firewall = new StrictHttpFirewall();
        firewall.setAllowUrlEncodedSlash(true);
        return firewall;
    }

    @Bean
    public CorsConfigurationSource corsConfigurationSource(ConfigurationManagementService configurationManagementService) {
        final CorsConfiguration configuration = new CorsConfiguration();
        final com.folib.configuration.CorsConfiguration internalCorsConfiguration = configurationManagementService
                .getConfiguration()
                .getCorsConfiguration();
        if (internalCorsConfiguration != null) {
            if (internalCorsConfiguration.getAllowedMethods() != null) {
                configuration.setAllowedMethods(new ArrayList<>(internalCorsConfiguration.getAllowedMethods()));
            }
            if (internalCorsConfiguration.getAllowedHeaders() != null) {
                configuration.setAllowedHeaders(new ArrayList<>(internalCorsConfiguration.getAllowedHeaders()));
            }
            if (internalCorsConfiguration.getAllowedOrigins() != null) {
                configuration.setAllowedOriginPatterns(new ArrayList<>(internalCorsConfiguration.getAllowedOrigins()));
            }
            if (internalCorsConfiguration.getExposedHeaders() != null) {
                configuration.setExposedHeaders(new ArrayList<>(internalCorsConfiguration.getExposedHeaders()));
            }
            if (internalCorsConfiguration.getAllowCredentials() != null) {
                configuration.setAllowCredentials(BooleanUtils.isTrue(internalCorsConfiguration.getAllowCredentials()));
            }
            if (internalCorsConfiguration.getMaxAge() != null) {
                configuration.setMaxAge(internalCorsConfiguration.getMaxAge());
            }
        }

        final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    @Bean
    AccessDeniedHandler accessDeniedHandler() {
        return new CustomAccessDeniedHandler();
    }

    @Bean
    @UnauthorizedEntyPoint
    AuthenticationEntryPoint customBasicAuthenticationEntryPoint() {
        return new PathSpecificBasicAuthenticationEntryPoint();
    }

    @Bean
    FolibAuthenticationFilter folibAuthenticationFilter() {
        return new FolibAuthenticationFilter(new AuthenticationSuppliers(suppliers), authenticationManager,customBasicAuthenticationEntryPoint(), configurationManager,customEntryPoint);
    }


    @Bean
    AnonymousAuthenticationFilter anonymousAuthenticationFilter() {
        List<GrantedAuthority> authorities = AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS");

        Role role = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
        authorities.addAll(role.getAccessModel().getApiAuthorities());
        return new AnonymousAuthenticationFilter("folib-unique-key",
                "anonymousUser",
                authorities);
    }

    //@Configuration
    //public static class MethodSecurityConfig {
    //
    //    @Inject
    //    MethodAccessDecisionManager methodAccessDecisionManager;
    //
    //    @Bean
    //    public MethodSecurityExpressionHandler methodSecurityExpressionHandler() {
    //        DefaultMethodSecurityExpressionHandler handler = new DefaultMethodSecurityExpressionHandler();
    //        // 根据需要配置表达式处理
    //        return handler;
    //    }
    //
    //    @Bean
    //    public AccessDecisionManager accessDecisionManager() {
    //        return methodAccessDecisionManager;
    //    }
    //}

    @Configuration
    public static class SharedObjectsConfig {

        @Bean
        AuthenticationTrustResolver authenticationTrustResolver() {
            return new AuthenticationTrustResolverImpl();
        }

    }

    @Qualifier
    public static @interface UnauthorizedEntyPoint {

    }

    @Bean
    public InitializingBean initializingBean() {
        return () -> SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
    }

    //  解决静态资源访问403,并对url 进行解码
    @Bean
    public WebSecurityCustomizer webSecurityCustomizer(HttpFirewall firewall) {
        return web -> web.httpFirewall(firewall);
    }

    @Bean
    public AuthorizationManagerBeforeMethodInterceptor extendedInterceptor(ExtendedAuthoritiesVoter extendedAuthoritiesVoter) {
        return AuthorizationManagerBeforeMethodInterceptor
                .preAuthorize(new ExtendedAuthorizationManager(extendedAuthoritiesVoter));
    }
}
