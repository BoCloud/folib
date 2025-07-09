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
package com.folib.authentication.api.jwt;

import java.util.Map;

import javax.inject.Inject;

import com.folib.security.exceptions.ExpiredTokenException;
import com.folib.security.exceptions.InvalidTokenException;
import com.folib.users.security.JwtAuthenticationClaimsProvider.JwtAuthentication;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.AbstractUserDetailsAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

/**
 * @author @author veadan
 *
 */

public class JwtAuthenticationProvider extends AbstractUserDetailsAuthenticationProvider
{

    @Inject
    private UserDetailsService userDetailsService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    private JwtClaimsProvider jwtClaimsProvider;

    public JwtAuthenticationProvider(@JwtAuthentication JwtClaimsProvider jwtClaimsProvider)
    {
        this.jwtClaimsProvider = jwtClaimsProvider;
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication)
        throws AuthenticationException
    {
        if (authentication.getCredentials() == null)
        {
            throw new BadCredentialsException("No credentials provided.");
        }

        String token = authentication.getCredentials().toString();

        Map<String, String> targetClaimMap = provideUserDetailsClaims(userDetails);
        try
        {
            securityTokenProvider.verifyToken(token, authentication.getPrincipal().toString(), targetClaimMap);
        }
        catch (ExpiredTokenException e)
        {
            throw new BadCredentialsException("expired");
        }
        catch (InvalidTokenException e)
        {
            logger.error(e.getMessage(), e);
            throw new BadCredentialsException("invalid.token");
        }

    }

    protected Map<String, String> provideUserDetailsClaims(UserDetails userDetails)
    {
        return jwtClaimsProvider.getClaims((SpringSecurityUser) userDetails);
    }

    @Override
    protected UserDetails retrieveUser(String username,
                                       UsernamePasswordAuthenticationToken authentication)
        throws AuthenticationException
    {
        UserDetails loadedUser;
        try
        {
            loadedUser = userDetailsService.loadUserByUsername(username);
        }
        catch (UsernameNotFoundException notFound)
        {
            throw notFound;
        }
        catch (Exception repositoryProblem)
        {
            throw new InternalAuthenticationServiceException(
                    repositoryProblem.getMessage(), repositoryProblem);
        }

        if (loadedUser == null)
        {
            throw new UsernameNotFoundException(String.format("User [%s] not found.", username));
        }
        return loadedUser;
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        //请注意是当前路径下的JwtAuthentication，不是JwtAuthenticationClaimsProvider.JwtAuthentication
        return com.folib.authentication.api.jwt.JwtAuthentication.class.isAssignableFrom(authentication);
    }

}
