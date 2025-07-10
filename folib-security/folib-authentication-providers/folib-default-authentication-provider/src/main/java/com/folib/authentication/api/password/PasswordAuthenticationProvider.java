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
package com.folib.authentication.api.password;

import com.folib.authentication.api.AuthenticationCache;
import com.folib.ldap.LdapAuthenticationConfigurationManager;
import com.folib.ldap.LdapConfiguration;
import com.folib.users.userdetails.SpringSecurityUser;
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

import javax.inject.Inject;
import java.text.MessageFormat;
import java.util.Optional;

/**
 * @author veadan
 * @author veadan
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
