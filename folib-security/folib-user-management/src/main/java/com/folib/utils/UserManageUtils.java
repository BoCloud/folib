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
package com.folib.utils;

import com.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/10
 **/
@Slf4j
public class UserManageUtils {

    /**
     * 获取登录用户名
     *
     * @return 用户名
     */
    public static String getUsername() {
        String username = "";
        try {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            if (Objects.isNull(securityContext)) {
                return username;
            }
            Authentication authentication = securityContext.getAuthentication();
            if (Objects.isNull(authentication)) {
                return username;
            }
            Object o = authentication.getPrincipal();
            String anonymousUser = "anonymousUser";
            if (anonymousUser.equals(o.toString())) {
                return anonymousUser;
            }
            if (!(o instanceof SpringSecurityUser)) {
                return username;
            }

            SpringSecurityUser userDetails = (SpringSecurityUser) o;
            username = userDetails.getUsername();
        } catch (Exception ex) {
            log.warn("获取登录用户名错误 [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return username;
    }

    /**
     * 获取登录用户
     *
     * @return 用户
     */
    public static SpringSecurityUser getSpringSecurityUser() {
        SpringSecurityUser springSecurityUser = null;
        try {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            if (Objects.isNull(securityContext)) {
                return null;
            }
            Authentication authentication = securityContext.getAuthentication();
            if (Objects.isNull(authentication)) {
                return null;
            }
            Object o = authentication.getPrincipal();
            String anonymousUser = "anonymousUser";
            if (anonymousUser.equals(o.toString())) {
                return null;
            }
            if (!(o instanceof SpringSecurityUser)) {
                return null;
            }
            return (SpringSecurityUser) o;
        } catch (Exception ex) {
            log.warn("获取登录用户错误 [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }
}
