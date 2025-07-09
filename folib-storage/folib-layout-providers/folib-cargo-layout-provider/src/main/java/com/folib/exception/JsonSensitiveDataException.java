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
package com.folib.exception;


import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.yaml.snakeyaml.scanner.ScannerException;

public class JsonSensitiveDataException extends JsonParsingException {
    private static final String SENSITIVE_DATA_REGEX = "([Pp]assword|[Kk]ey)(:.+$)";

    private static final Pattern PATTERN = Pattern.compile("([Pp]assword|[Kk]ey)(:.+$)", 8);

    private static final String MASK = ": ******";

    public JsonSensitiveDataException(Exception e) {
        super(maskSensitiveInputs(e));
        if (isCausedByScannerExceptionWithSensitiveData(e)) {
            Throwable rootCause = Optional.<Throwable>ofNullable(ExceptionUtils.getRootCause(e)).orElse(e);
            setStackTrace(rootCause.getStackTrace());
        } else {
            initCause(e);
        }
    }

    private static String maskSensitiveInputs(Exception e) {
        String result = "";
        if (e != null) {
            String message = e.toString();
            if (StringUtils.isNotBlank(message)) {
                result = message;
                if (isCausedByScannerExceptionWithSensitiveData(e)) {
                    Matcher matcher = PATTERN.matcher(message);
                    while (matcher.find())
                        result = result.replace(matcher.group(2), ": ******");
                }
            }
        }
        return result;
    }

    private static boolean isCausedByScannerExceptionWithSensitiveData(Exception e) {
        boolean result = false;
        if (e != null && ExceptionUtils.indexOfType(e, ScannerException.class) != -1)
            result = recursivelyCheckForSensitiveData(e);
        return result;
    }

    private static boolean recursivelyCheckForSensitiveData(Throwable e) {
        boolean result = false;
        String message = e.toString();
        if (StringUtils.isNotBlank(message)) {
            Matcher matcher = PATTERN.matcher(message);
            if (matcher.find()) {
                result = true;
            } else if (e.getCause() != null) {
                result = recursivelyCheckForSensitiveData(e.getCause());
            }
        }
        return result;
    }
}

