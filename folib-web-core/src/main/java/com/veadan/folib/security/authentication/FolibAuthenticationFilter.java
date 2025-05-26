package com.veadan.folib.security.authentication;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.authentication.DatabaseExternalUsersCacheManager;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.userdetails.DataBaseUserDetailService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.HttpHeaders;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.context.SecurityContextRepository;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;


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
                                     AuthenticationEntryPoint authenticationEntryPoint) {
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
            "/rest/**"
    );

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
            // 仅对非排除路径执行认证逻辑
            if (!shouldNotFilter(request)) {
                // 执行自定义认证逻辑（例如解析 Token、设置 SecurityContext 等）
                Authentication authentication = authenticationSuppliers.supply(request);
                if (authentication == null) {
                    authentication = SecurityContextHolder.getContext().getAuthentication();
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

                    //校验token是否有效
                    // Token 无效，返回 401 Unauthorized
                    response.getWriter().write("Invalid or expired token");
                    response.setContentType("application/json");
                    response.setStatus(HttpStatus.UNAUTHORIZED.value());
                    logger.error("authentication 无效，返回 401 Unauthorized");
                    return;

                }
            }
            // 继续执行后续过滤器
            filterChain.doFilter(request, response);
        } catch (AuthenticationException authException) {
            authenticationEntryPoint.commence(request, response, authException);
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
