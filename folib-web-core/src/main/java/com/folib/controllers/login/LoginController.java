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
package com.folib.controllers.login;

import com.folib.controllers.BaseController;
import com.folib.security.authentication.JwtTokenFetcher;
import com.folib.security.authentication.suppliers.JsonFormLoginSupplier;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import org.jose4j.lang.JoseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import java.util.Map;
import java.util.Optional;

import static com.folib.controllers.login.LoginController.REQUEST_MAPPING;
import static com.folib.users.security.JwtAuthenticationClaimsProvider.JwtAuthentication;

/**
 * Works in conjunction with {@link JsonFormLoginSupplier}
 *
 * @author veadan
 */
@RestController
@RequestMapping(value = REQUEST_MAPPING)
@Api(description = "用户登录控制器", tags = "用户登录控制器")
public class LoginController
        extends BaseController {

    public static final String REQUEST_MAPPING = "/api/login";

    private static final Logger logger = LoggerFactory.getLogger(LoginController.class);

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @ApiOperation(value = "返回提供的用户名和密码的 JWT 身份验证令牌")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "Returns generated JWT token"),
            @ApiResponse(code = 401, message = "Invalid credentials"),
            @ApiResponse(code = 500, message = "org.springframework.security.core.Authentication " +
                    "fetched by the folib security implementation " +
                    "is not supported")})
//  @PreAuthorize("hasAuthority('UI_LOGIN')")
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity login(Authentication authentication) {
        return formLogin(authentication);
    }



    @ApiOperation(value = "返回提供的用户名和密码的 JWT 身份验证令牌")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "Returns generated JWT token"),
            @ApiResponse(code = 401, message = "Invalid credentials"),
            @ApiResponse(code = 500, message = "org.springframework.security.core.Authentication " +
                    "fetched by the folib security implementation " +
                    "is not supported")})
//    @PreAuthorize("hasAuthority('UI_LOGIN')")
    @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<?> formLogin(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            throw new InsufficientAuthenticationException("unauthorized");
        }
        if (!(authentication instanceof UsernamePasswordAuthenticationToken)) {
            return toResponseEntityError("Unsupported authentication class " + authentication.getClass().getName());
        }

        Object principal = authentication.getPrincipal();
        if (!(principal instanceof SpringSecurityUser)) {
            return toResponseEntityError("Unsupported authentication principal " + Optional.ofNullable(principal).orElse(null));
        }

        String token;
        try {

            SpringSecurityUser user = (SpringSecurityUser) principal;
            String subject = user.getUsername();

            Integer timeout = configurationManager.getSessionTimeoutSeconds();
            Map<String, String> claims = jwtClaimsProvider.getClaims(user);
            token = securityTokenProvider.getToken(subject, claims, timeout, null);
            //存储JWT令牌到Cookie
            Cookie cookie = new Cookie(JwtTokenFetcher.AUTHORIZATION_COOKIE, token);
            cookie.setPath("/");
            cookie.setMaxAge(timeout);
            HttpServletResponse response = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getResponse();
            response.addCookie(cookie);
        } catch (JoseException e) {
            logger.error("Unable to create JWT token.", e);

            return toResponseEntityError("Unable to create JWT token.", HttpStatus.BAD_REQUEST);
        }
        return ResponseEntity.ok().body(new LoginOutput(token, authentication.getAuthorities()));
    }


////    @PreAuthorize("hasAuthority('UI_LOGIN')")
//    @ApiOperation(value = "Returns the JWT authentication token for provided username and password")
//    @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE,value = "/remote")
//    public ResponseEntity formLoginRemote(@RequestBody LoginParam loginParam)
//    {
//        if (loginParam == null||loginParam.getUsername()==null||loginParam.getPassword()==null)
//        {
//            throw new InsufficientAuthenticationException("unauthorized");
//        }
//        Authentication authentication = new PasswordAuthentication(loginParam.getUsername(),loginParam.getPassword());
//        String token;
//        try
//        {
//
//            SpringSecurityUser user = (SpringSecurityUser) authentication.getPrincipal();;
//            String subject = user.getUsername();
//
//            Integer timeout = configurationManager.getSessionTimeoutSeconds();
//            Map<String, String> claims = jwtClaimsProvider.getClaims(user);
//            token = securityTokenProvider.getToken(subject, claims, timeout, null);
//        }
//        catch (JoseException e)
//        {
//            logger.error("Unable to create JWT token.", e);
//
//            return toResponseEntityError("Unable to create JWT token.", HttpStatus.BAD_REQUEST);
//        }
//
//        return ResponseEntity.ok().body(new LoginOutput(token, authentication.getAuthorities()));
//    }

}
