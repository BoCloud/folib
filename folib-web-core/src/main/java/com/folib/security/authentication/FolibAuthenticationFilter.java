package com.folib.security.authentication;

import com.folib.security.authentication.Http401AuthenticationEntryPoint;
import com.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.folib.configuration.ConfigurationManager;
import com.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.folib.storage.repository.RepositoryData;
import com.folib.utils.RequestUtils;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.context.SecurityContextRepository;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;


import java.io.IOException;
import java.util.List;
import java.util.Optional;



/**
 * @author veadan
 */
public class FolibAuthenticationFilter
        extends OncePerRequestFilter {

    private static final Logger logger = LoggerFactory.getLogger(FolibAuthenticationFilter.class);

    private final AuthenticationManager authenticationManager;

    private final AuthenticationSuppliers authenticationSuppliers;

    private final AuthenticationEntryPoint authenticationEntryPoint;


    public FolibAuthenticationFilter(AuthenticationSuppliers authenticationSuppliers,
                                     AuthenticationManager authenticationManager,
                                     AuthenticationEntryPoint authenticationEntryPoint){
        super();
        this.authenticationSuppliers = authenticationSuppliers;
        this.authenticationManager = authenticationManager;
        this.authenticationEntryPoint=authenticationEntryPoint;
    }
    // 需要跳过的路径列表（与 SecurityConfig 中的路径一致）
    private static final List<String> EXCLUDED_PATHS = List.of(
            "/favicon.ico",
            "/ui/**",
            "/docs/**",
            "/webjars/**",
            "/rest/**",
            "/help/**",
            "/v2/",
            "/api/configuration/cluster/**",
            "/"
    );

    private  static final  List<String> ANONYMOUS_URL = List.of("/storages/**","/api/browse/**");

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        // 使用 AntPathMatcher 检查当前请求是否匹配排除路径
        AntPathMatcher matcher = new AntPathMatcher();
        return EXCLUDED_PATHS.stream()
                .anyMatch(pattern -> matcher.match(pattern, request.getServletPath()));
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
            throws ServletException,
            IOException {
        try {

            Authentication authentication = authenticationSuppliers.supply(request);
            if (authentication == null) {
                //authentication = SecurityContextHolder.getContext().getAuthentication();
                logger.debug("Authentication not supplied by any authentication supplier, using [{}] context authentication.",
                        Optional.ofNullable(authentication).map(a -> a.getClass().getSimpleName()).orElse("empty"));
            } else {
                logger.debug("Supplied [{}] authentication.", authentication.getClass().getSimpleName());
            }

            authentication = provideAuthentication(authentication);
            if (authentication != null) {
                SecurityContext context = SecurityContextHolder.createEmptyContext();
                context.setAuthentication(authentication);
                SecurityContextHolder.setContext(context);
                // 显式保存认证信息
                SecurityContextRepository repo = new HttpSessionSecurityContextRepository();
                repo.saveContext(context, request, response);
            } else {

                SecurityContextHolder.clearContext();
                SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
            }
            // 继续执行后续过滤器
            filterChain.doFilter(request, response);
        } catch (AuthenticationException authException) {
            authenticationEntryPoint.commence(request, response, authException);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }

    }

    private Authentication provideAuthentication(Authentication authentication) {
        String authenticationName = Optional.ofNullable(authentication)
                .map(a -> a.getClass().getSimpleName())
                .orElse("empty");
        if (authentication == null || authentication.isAuthenticated()) {
            logger.debug("Authentication {} already authenticated or empty, skip providers.", authenticationName);

            return authentication;
        }

        Authentication authResult = authenticationManager.authenticate(authentication);
        logger.debug("Authenticated with {}", authenticationName);

        return authResult;
    }



}
