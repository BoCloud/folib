package com.veadan.folib.security.vote;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.aopalliance.intercept.MethodInvocation;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UrlUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.ExpressionBasedPreInvocationAdvice;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdviceVoter;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

/**
 * @author sbespalov
 *
 */
@Component
public class ExtendedAuthoritiesVoter extends PreInvocationAuthorizationAdviceVoter
{
    private final Logger logger = LoggerFactory.getLogger(ExtendedAuthoritiesVoter.class);

    public ExtendedAuthoritiesVoter()
    {
        super(new ExpressionBasedPreInvocationAdvice());
    }
    
    @Override
    public int vote(Authentication authentication,
                    MethodInvocation method,
                    Collection<ConfigAttribute> attributes)
    {
        return super.vote(new ExtendedAuthorityAuthentication(authentication), method, attributes);
    }

    @SuppressWarnings("serial")
    private class ExtendedAuthorityAuthentication implements Authentication
    {

        private Authentication source;

        public ExtendedAuthorityAuthentication(Authentication target)
        {
            super();
            this.source = target;
        }

        private Authentication getSourceAuthentication()
        {
            return source;
        }

        private Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication)
        {
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> apiAuthorities = authentication.getAuthorities();
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);

            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken)
            {

                return authentication.getAuthorities();
            }
            else if (!(principal instanceof SpringSecurityUser))
            {

                logger.warn("Unknown authentication principal type [{}]", principal.getClass());

                return authentication.getAuthorities();
            }

            String requestUri = UrlUtils.getRequestUri();
            if (!requestUri.startsWith(ARTIFACT_ROOT_PATH))
            {
                return apiAuthorities;
            }

            String storageId = UrlUtils.getCurrentStorageId();
            String repositoryId = UrlUtils.getCurrentRepositoryId();
            if (storageId == null || repositoryId == null)
            {
                return apiAuthorities;
            }

            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            // calculate privileges based on roles access model
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(UrlUtils.getRequestUri());
            if (storageAuthorities.isEmpty())
            {
                return apiAuthorities;
            }

            List<GrantedAuthority> extendedAuthorities = new ArrayList<>(apiAuthorities);
            extendedAuthorities.addAll(storageAuthorities);
            logger.debug("Privileges for [{}] was extended to [{}]", userDetails.getUsername(), extendedAuthorities);

            return extendedAuthorities;
        }

        public String getName()
        {
            return getSourceAuthentication().getName();
        }

        public Collection<? extends GrantedAuthority> getAuthorities()
        {
            return calculateExtendedAuthorities(getSourceAuthentication());
        }

        public Object getCredentials()
        {
            return getSourceAuthentication().getCredentials();
        }

        public Object getDetails()
        {
            return getSourceAuthentication().getDetails();
        }

        public Object getPrincipal()
        {
            return getSourceAuthentication().getPrincipal();
        }

        public boolean isAuthenticated()
        {
            return getSourceAuthentication().isAuthenticated();
        }

        public void setAuthenticated(boolean isAuthenticated)
            throws IllegalArgumentException
        {
            getSourceAuthentication().setAuthenticated(isAuthenticated);
        }

    }
}
