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

import com.folib.enums.VersionConditionTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.util.Arrays;
import java.util.List;

/**
 * @author veadan
 * @date 2023/10/25
 **/
@Slf4j
public class VersionUtils {

    /**
     * 版本号比较
     *
     * @param v1 值1
     * @param v2 值1
     * @return 0代表相等，1代表左边大，-1代表右边大
     */
    public static int compareVersion(String v1, String v2) {
        if (v1.equalsIgnoreCase(v2)) {
            return 0;
        }
        String[] version1Array = v1.split("[._-]");
        String[] version2Array = v2.split("[._-]");
        int index = 0;
        int minLen = Math.min(version1Array.length, version2Array.length);
        long diff = 0;

        while (index < minLen) {
            String val1 = version1Array[index];
            String val2 = version2Array[index];
            if (!StringUtils.isNumeric(val1) && !StringUtils.isNumeric(val2)) {
                diff = val1.compareToIgnoreCase(val2);
                if (diff == 0) {
                    index++;
                    continue;
                } else {
                    break;
                }
            }
            if (!StringUtils.isNumeric(val1) && StringUtils.isNumeric(val2)) {
                diff = -1;
                break;
            } else if (StringUtils.isNumeric(val1) && !StringUtils.isNumeric(val2)) {
                diff = 1;
                break;
            }
            if ((diff = Long.parseLong(val1)
                    - Long.parseLong(val2)) == 0) {
                index++;
            } else {
                break;
            }
        }
        if (diff == 0) {
            for (int i = index; i < version1Array.length; i++) {
                String var = version1Array[i];
                if (!StringUtils.isNumeric(var)) {
                    return 1;
                } else if (StringUtils.isNumeric(var) && Long.parseLong(var) > 0) {
                    return 1;
                }
            }
            for (int i = index; i < version2Array.length; i++) {
                String var = version2Array[i];
                if (!StringUtils.isNumeric(var)) {
                    return -1;
                } else if (StringUtils.isNumeric(var) && Long.parseLong(var) > 0) {
                    return -1;
                }
            }
            return 0;
        } else {
            return diff > 0 ? 1 : -1;
        }
    }

    /**
     * 比较版本是否在范围内
     *
     * @param version 版本
     * @param range   范围
     * @return true 在范围内 false 不在范围内
     */
    public static boolean versionInRange(String version, String range) {
        try {
            List<String> rangeList = parseRangeExpression(range);
            if (CollectionUtils.isEmpty(rangeList)) {
                return false;
            }
            String asterisk = "*", startSymbol = range.substring(0, 1), endSymbol = range.substring(range.length() - 1);
            String startVersion = rangeList.get(0);
            String endVersion = rangeList.get(1);
            int result;
            List<Integer> valueList;
            if (!startVersion.equals(asterisk)) {
                valueList = VersionConditionTypeEnum.queryValueBySymbol(startSymbol);
                result = compareVersion(version, startVersion);
                if (!valueList.contains(result)) {
                    return false;
                }
            }
            if (!endVersion.equals(asterisk)) {
                valueList = VersionConditionTypeEnum.queryValueBySymbol(endSymbol);
                result = compareVersion(version, endVersion);
                if (!valueList.contains(result)) {
                    return false;
                }
            }
            return true;
        } catch (Exception ex) {
            log.warn("判断版本异常：[{}]", ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    /**
     * 范围转换为集合
     *
     * @param expression 范围
     * @return 集合
     */
    public static List<String> parseRangeExpression(String expression) {
        expression = expression.replaceAll("\\(", "");
        expression = expression.replaceAll("\\[", "");
        expression = expression.replaceAll("\\)", "");
        expression = expression.replaceAll("]", "");
        List<String> rangeList = Arrays.asList(expression.split(","));
        int two = 2;
        if (CollectionUtils.isEmpty(rangeList) || rangeList.size() != two) {
            return null;
        }
        return rangeList;
    }

}


