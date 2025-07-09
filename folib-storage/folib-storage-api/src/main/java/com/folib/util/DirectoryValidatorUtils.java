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
package com.folib.util;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.components.DistributedCacheComponent;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2025/2/24
 **/
@Slf4j
public class DirectoryValidatorUtils {

    /**
     * 自定义黑名单字符，路径不允许存在\和?
     */
    private static final String INVALID_CHARS = "\\\\?";

    private static final Pattern INVALID_PATTERN = Pattern.compile("[" + Pattern.quote(INVALID_CHARS) + "]");

    public static boolean validateDirectoryPath(String path) {
        try {
            if (StringUtils.isBlank(path)) {
                return true;
            }
            if (path.trim().isEmpty()) {
                log.error("目录名不能全是空格");
                return false;
            }
            // 检查黑名单字符
            if (getInvalidPattern().matcher(path).find()) {
                log.error("目录名包含非法字符: [{}]", path);
                return false;
            }
            // 检查操作系统路径合法性
            try {
                Paths.get(path);
            } catch (InvalidPathException ex) {
                log.error("非法路径 name [{}] error [{}]", path, ex.getMessage());
                return false;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return true;
    }

    public static Pattern getInvalidPattern() {
        String pathInvalidPatternKey = "PATH_INVALID_CHARS";
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String pathInvalidPatternValue = distributedCacheComponent.get(pathInvalidPatternKey);
        if (StringUtils.isBlank(pathInvalidPatternValue)) {
            return INVALID_PATTERN;
        }
        return Pattern.compile("[" + Pattern.quote(pathInvalidPatternValue) + "]");
    }
}
