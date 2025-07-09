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
package com.folib.common;

import java.util.Collection;
import javax.annotation.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.AntPathMatcher;

public abstract class PathMatcher {
    private static final Logger log = LoggerFactory.getLogger(PathMatcher.class);

    private static final AntPathMatcher antPathMatcher = new AntPathMatcher();

    static {
        antPathMatcher.setTrimTokens(true);
    }

    public static boolean matches(String path, @Nullable Collection<String> includes, @Nullable Collection<String> excludes, boolean isFolder) {
        if (notNullOrEmpty(excludes)) {
            for (String exclude : excludes) {
                if (antPathMatcher.match(exclude, path)) {
                    log.debug("excludes pattern ({}) rejected path '{}'.", exclude, path);
                    return false;
                }
            }
        }
        if (notNullOrEmpty(includes)) {
            for (String include : includes) {
                if (includeMatch(path, isFolder, include)) {
                    return true;
                }
            }
        } else {
            return true;
        }
        return false;
    }

    public static boolean matches(String pattern, String path) {
        return antPathMatcher.match(pattern, path);
    }

    private static boolean includeMatch(String path, boolean useStartMatch, String include) {
        return ("**/*".equals(include) || "**"
                .equals(include) || (useStartMatch && antPathMatcher
                .matchStart(include, path)) || antPathMatcher
                .match(include, path));
    }

    private static boolean notNullOrEmpty(Collection<String> pattern) {
        return (pattern != null && !pattern.isEmpty());
    }
}

