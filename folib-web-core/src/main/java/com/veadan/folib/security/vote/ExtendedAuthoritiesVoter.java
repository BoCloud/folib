package com.veadan.folib.security.vote;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.veadan.folib.controllers.BrowseController;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UrlUtils;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.ExpressionBasedPreInvocationAdvice;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdviceVoter;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.TimeUnit;

import static com.veadan.folib.web.Constants.*;

/**
 * @author xuxinping
 */
@Component
public class ExtendedAuthoritiesVoter extends PreInvocationAuthorizationAdviceVoter {

    private final Logger logger = LoggerFactory.getLogger(ExtendedAuthoritiesVoter.class);

    private final Cache<String, Boolean> repositoryAllowAnonymousCache = CacheBuilder.newBuilder()
            .expireAfterWrite(5, TimeUnit.MINUTES)
            .build();

    @Autowired
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    public ExtendedAuthoritiesVoter() {
        super(new ExpressionBasedPreInvocationAdvice());
    }

    @Override
    public int vote(Authentication authentication,
                    MethodInvocation method,
                    Collection<ConfigAttribute> attributes) {
        return super.vote(new ExtendedAuthorityAuthentication(authentication), method, attributes);
    }

    @SuppressWarnings("serial")
    private class ExtendedAuthorityAuthentication implements Authentication {

        private Authentication source;

        public ExtendedAuthorityAuthentication(Authentication target) {
            super();
            this.source = target;
        }

        private Authentication getSourceAuthentication() {
            return source;
        }

        private Boolean getRepositoryAllowAnonymousFromCacheOrLoad(String storageId, String repositoryId) {
            String key = String.format("%s:%s", storageId, repositoryId);
            Boolean cacheAllowAnonymous = repositoryAllowAnonymousCache.getIfPresent(key);
            if (Objects.isNull(cacheAllowAnonymous)) {
                final boolean allowAnonymous = configurationManagementService.getConfiguration().getRepository(storageId, repositoryId).isAllowAnonymous();
                cacheAllowAnonymous = allowAnonymous;
                repositoryAllowAnonymousCache.put(key, allowAnonymous);
            }
            return cacheAllowAnonymous;
        }

        private Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication) {
            String storageId = UrlUtils.getCurrentStorageId();
            String repositoryId = UrlUtils.getCurrentRepositoryId();
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> apiAuthorities = authentication.getAuthorities();
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);
            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    if (Boolean.FALSE.equals(getRepositoryAllowAnonymousFromCacheOrLoad(storageId, repositoryId))) {
                        return Collections.emptySet();
                    }
                }
                return authentication.getAuthorities();
            } else if (!(principal instanceof SpringSecurityUser)) {
                logger.warn("Unknown authentication principal type [{}]", principal.getClass());
                return authentication.getAuthorities();
            }
            String requestUri = UrlUtils.getRequestUri();
            List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
            if (paths.stream().noneMatch(requestUri::startsWith)) {
                return apiAuthorities;
            }
            if (storageId == null || repositoryId == null) {
                return apiAuthorities;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(requestUri);
            if (storageAuthorities.isEmpty()) {
                return apiAuthorities;
            }
            List<GrantedAuthority> extendedAuthorities = new ArrayList<>(apiAuthorities);
            extendedAuthorities.addAll(storageAuthorities);
            logger.debug("Privileges for [{}] was extended to [{}]", userDetails.getUsername(), extendedAuthorities);
            return extendedAuthorities;
        }

        @Override
        public String getName() {
            return getSourceAuthentication().getName();
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return calculateExtendedAuthorities(getSourceAuthentication());
        }

        @Override
        public Object getCredentials() {
            return getSourceAuthentication().getCredentials();
        }

        @Override
        public Object getDetails() {
            return getSourceAuthentication().getDetails();
        }

        @Override
        public Object getPrincipal() {
            return getSourceAuthentication().getPrincipal();
        }

        @Override
        public boolean isAuthenticated() {
            return getSourceAuthentication().isAuthenticated();
        }

        @Override
        public void setAuthenticated(boolean isAuthenticated)
                throws IllegalArgumentException {
            getSourceAuthentication().setAuthenticated(isAuthenticated);
        }

    }
}
