package com.veadan.folib.authentication.api.password;

import com.veadan.folib.authentication.api.AuthenticationCache;
import com.veadan.folib.authentication.api.ldap.LdapAuthenticationConfigurationManager;
import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.enums.LoginTypeEnum;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.Optional;

/**
 * @author veadan
 * @author xuxinping
 */
public class PasswordAuthenticationProvider extends DaoAuthenticationProvider {

    private static final Logger logger = LoggerFactory.getLogger(PasswordAuthenticationProvider.class);

    @Inject
    private AuthenticationCache authenticationCache;

    @Inject
    private LdapTemplate ldapTemplate;

    @Inject
    private LdapAuthenticationConfigurationManager ldapAuthenticationConfigurationManager;

    @Override
    @Inject
    public void setPasswordEncoder(PasswordEncoder passwordEncoder) {
        super.setPasswordEncoder(passwordEncoder);
    }

    @Override
    @Inject
    public void setUserDetailsService(UserDetailsService userDetailsService) {
        super.setUserDetailsService(userDetailsService);
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return PasswordAuthentication.class.isAssignableFrom(authentication);
    }

    @Override
    public Authentication authenticate(Authentication authentication)
            throws AuthenticationException {
        try {
            return super.authenticate(authentication);
        } catch (BadCredentialsException e) {
            throw new BadCredentialsException("invalid.credentials");
        }
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication)
            throws AuthenticationException {
        if (userDetails instanceof SpringSecurityUser) {
            SpringSecurityUser ldapUser = (SpringSecurityUser) userDetails;
            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            if (ldapUserDetailsServiceSourceId.equalsIgnoreCase(ldapUser.getSourceId())) {
                //ldap
                logger.info("The account [{}] belongs to the ldapUser", userDetails.getUsername());
                LdapConfiguration ldapConfiguration = ldapAuthenticationConfigurationManager.getConfiguration();
                String ldapUserSearchFilter = MessageFormat.format(ldapConfiguration.getUserSearch().getUserSearchFilter(), userDetails.getUsername());
                logger.info("The ldap user search base [{}] search filter [{}]", ldapConfiguration.getUserSearch().getUserSearchBase(), ldapUserSearchFilter);
                ldapTemplate.setIgnorePartialResultException(true);
                boolean authenticate = ldapTemplate.authenticate(ldapConfiguration.getUserSearch().getUserSearchBase(), ldapUserSearchFilter, authentication.getCredentials().toString());
                logger.info("The ldap account [{}] password authenticate [{}]", userDetails.getUsername(), authenticate);
                if (authenticate) {
                    return;
                }
                throw new BadCredentialsException("invalid.credentials");
            }
        }
        UsernamePasswordAuthenticationToken cachedAuthentication = authenticationCache.getAuthenticationToken(userDetails.getUsername());

        if (Optional.ofNullable(cachedAuthentication)
                .filter(c -> authentication.getCredentials() != null && c.getCredentials() != null)
                .filter(c -> authenticationCache.matches(authentication.getCredentials()
                                .toString(),
                        c.getCredentials()
                                .toString()))
                .isPresent()) {
            logger.info("Found cached authentication for [{}]", userDetails.getUsername());
            return;
        }

        try {
            super.additionalAuthenticationChecks(userDetails, authentication);
        } catch (BadCredentialsException e) {
            logger.warn("User [{}] verification fails for [{}].", userDetails.getUsername(), authentication.getClass().getSimpleName());
            throw new BadCredentialsException("invalid.credentials");
        }

        authenticationCache.putAuthenticationToken(authentication);
    }

}
