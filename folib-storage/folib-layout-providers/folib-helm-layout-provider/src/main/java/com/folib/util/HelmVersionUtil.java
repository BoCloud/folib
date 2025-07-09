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


import org.apache.commons.lang.StringUtils;

public class HelmVersionUtil {
    public static final String PATTERN_TO_MARK_VERSION = "___VERSIONPROP___";

    static String addQuotesToVersionsAttributes(String yaml) {
        if (StringUtils.isEmpty(yaml)) {
            return yaml;
        }
        return yaml.replace("___VERSIONPROP___", "\"");
    }

    //public static String markWithReplacePattern(String version) {
    //    StringQuotingChecker.Default default_ = new StringQuotingChecker.Default();
    //    version = removeUnfinishedSuffixIfNeeded(version, "\"");
    //    version = removeUnfinishedSuffixIfNeeded(version, "'");
    //    if (StringUtils.isEmpty(version) || version.startsWith("\"") || version.startsWith("'") || version.startsWith("$") || default_
    //            .needToQuoteValue(version)) {
    //        return version;
    //    }
    //    return "___VERSIONPROP___" + version + "___VERSIONPROP___";
    //}
    //
    //private static String removeUnfinishedSuffixIfNeeded(String version, String quoteCharToRemove) {
    //    if (StringUtils.isNotBlank(version) && version.endsWith(quoteCharToRemove) && !version.startsWith(quoteCharToRemove)) {
    //        version = StringUtils.removeEnd(version, quoteCharToRemove);
    //    }
    //    return version;
    //}

    public static String markWithReplacePattern(String version) {
        // 移除未完成的后缀
        version = removeUnfinishedSuffixIfNeeded(version, "\"");
        version = removeUnfinishedSuffixIfNeeded(version, "'");

        // 检查是否需要标记
        if (StringUtils.isEmpty(version) ||
                version.startsWith("\"") ||
                version.startsWith("'") ||
                version.startsWith("$") ||
                needsToBeQuoted(version)) {
            return version;
        }

        // 标记字符串
        return  version;
    }

    private static String removeUnfinishedSuffixIfNeeded(String version, String quoteChar) {
        if (version.endsWith(quoteChar) && !version.endsWith(quoteChar + quoteChar)) {
            return version.substring(0, version.length() - 1);
        }
        return version;
    }

    private static boolean needsToBeQuoted(String value) {
        // 检查是否包含特殊字符，如空格、逗号等
        return value.contains(" ") || value.contains(",") || value.contains(":");
    }
}
