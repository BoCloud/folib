package com.veadan.folib.security.vote;

import com.veadan.folib.controllers.BrowseController;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.veadan.folib.web.Constants.*;

/**
 * @author xuxinping
 */
@Component
public class ExtendedAuthoritiesVoter extends PreInvocationAuthorizationAdviceVoter {
    private final Logger logger = LoggerFactory.getLogger(ExtendedAuthoritiesVoter.class);

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

        private Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication) {
            String storageId = UrlUtils.getCurrentStorageId();
            String repositoryId = UrlUtils.getCurrentRepositoryId();
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> apiAuthorities = authentication.getAuthorities();
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);

            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
                //匿名访问
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    StorageDto storageDto = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                    if (Objects.nonNull(storageDto)) {
                        RepositoryDto repositoryDto = storageDto.getRepository(repositoryId);
                        if (Objects.nonNull(repositoryDto) && Boolean.FALSE.equals(repositoryDto.isAllowAnonymous())) {
                            //不允许匿名访问
                            return authentication.getAuthorities().stream().filter(item -> !Privileges.anonymous().contains(item.getAuthority())).collect(Collectors.toList());
                        }
                    }
                }
                return authentication.getAuthorities();
            } else if (!(principal instanceof SpringSecurityUser)) {

                logger.warn("Unknown authentication principal type [{}]", principal.getClass());

                return authentication.getAuthorities();
            }

            String requestUri = UrlUtils.getRequestUri();
            if (!requestUri.startsWith(ARTIFACT_ROOT_PATH) && !requestUri.startsWith(DOCKER_ROOT_PATH) && !requestUri.startsWith(BrowseController.ROOT_CONTEXT) && !requestUri.startsWith(STORAGE_ROOT_PATH)) {
                return apiAuthorities;
            }

            if (storageId == null || repositoryId == null) {
                return apiAuthorities;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            // calculate privileges based on roles access model
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(UrlUtils.getRequestUri());
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
