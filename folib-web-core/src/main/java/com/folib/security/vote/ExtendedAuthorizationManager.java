package com.folib.security.vote;

import cn.hutool.core.collection.CollectionUtil;
import com.folib.security.vote.ExtendedAuthoritiesVoter;
import com.folib.utils.RequestUtils;
import lombok.extern.slf4j.Slf4j;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.core.Authentication;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ExtendedAuthorizationManager implements AuthorizationManager<MethodInvocation> {


    private final ExtendedAuthoritiesVoter extendedAuthoritiesVoter;

    public ExtendedAuthorizationManager(ExtendedAuthoritiesVoter extendedAuthoritiesVoter) {
        this.extendedAuthoritiesVoter = extendedAuthoritiesVoter;
    }

    @Override
    public AuthorizationDecision check(Supplier<Authentication> authenticationSupplier, MethodInvocation methodInvocation) {
        Authentication authentication = null;
        try {

            // 从方法上提取 PreAuthorize 注解中的表达式
            Method method = methodInvocation.getMethod();
            PreAuthorize preAuthorize = method.getAnnotation(PreAuthorize.class);
            List<String> requiredAuthority = null;
            if (preAuthorize != null) {
                authentication = getAuthentication();
                String expr = preAuthorize.value();
                ParsedAuthorization parsed = parseAuthorizationExpression(expr);
                if (parsed.type() == AuthorizationType.AUTHENTICATED) {
                    // 检查 authentication.isAuthenticated() && !(instanceof AnonymousToken)
                    return new AuthorizationDecision(authentication.isAuthenticated() && !(authentication instanceof AnonymousAuthenticationToken));
                } else if (parsed.type() == AuthorizationType.AUTHORITY) {
                    // 检查是否有指定权限
                    requiredAuthority = parsed.values();
                }
            }else {
                return new AuthorizationDecision(true);
            }
            // 如果没有显式注解就默认拒绝
            if (requiredAuthority == null) {
                return new AuthorizationDecision(true);
            }
            String storageId = RequestUtils.getStorageId();
            String repositoryId = RequestUtils.getRepositoryId();
            boolean granted;
            Collection<String> extendedAuthorities = extendedAuthoritiesVoter
                    .getExtendedAuthorities(authentication, storageId, repositoryId, null);
            //判断是否匿名用户
            if (!CollectionUtil.isEmpty(extendedAuthorities) && (authentication == null || !authentication.isAuthenticated() ||
                    authentication instanceof AnonymousAuthenticationToken)) {
                SecurityContextHolder.clearContext();
                SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
                // 设置匿名身份 用于 @PreAuthorize("hasAuthority('')")
                List<String> anonymousAuthorities = extendedAuthorities != null ? extendedAuthorities.stream().toList() : requiredAuthority;
                authentication = new AnonymousAuthenticationToken(
                        "anonymousUser",
                        "anonymousUser",
                        AuthorityUtils.createAuthorityList(anonymousAuthorities)
                );
                log.info("Using anonymous authentication");
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
            granted = extendedAuthorities != null && requiredAuthority.stream().anyMatch(extendedAuthorities::contains);
            return new AuthorizationDecision(granted);
        } catch (RuntimeException e) {
            log.error("Error while checking authorization", e.fillInStackTrace());
            return new AuthorizationDecision(false);
        }
    }

    public Authentication getAuthentication() {
        Authentication authentication =  SecurityContextHolder.getContext().getAuthentication();
        if(authentication ==null){
            authentication = new AnonymousAuthenticationToken(
                    "anonymousUser",
                    "anonymousUser",
                    AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS")
            );
        }
        return authentication;
    }


    private record ParsedAuthorization(AuthorizationType type, List<String> values) {
    }

    private ParsedAuthorization parseAuthorizationExpression(String expression) {
        if (expression == null || expression.isBlank()) {
            return new ParsedAuthorization(AuthorizationType.UNKNOWN, List.of());
        }

        if (expression.trim().equals("authenticated")) {
            return new ParsedAuthorization(AuthorizationType.AUTHENTICATED, List.of());
        }

        Pattern pattern = Pattern.compile("has(?:Any)?Authority\\(([^)]+)\\)");
        Matcher matcher = pattern.matcher(expression);
        if (matcher.find()) {
            String group = matcher.group(1);
            String[] parts = group.split(",");
            List<String> authorities = Arrays.stream(parts)
                    .map(s -> s.trim().replaceAll("^['\"]|['\"]$", ""))
                    .toList();
            return new ParsedAuthorization(AuthorizationType.AUTHORITY, authorities);
        }

        return new ParsedAuthorization(AuthorizationType.UNKNOWN, List.of());
    }

    private enum AuthorizationType {
        AUTHORITY, AUTHENTICATED, UNKNOWN
    }

}
