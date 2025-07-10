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


import com.google.common.collect.Range;

/**
 * @author veadan
 * @date 2023/10/24
 **/
public class RangeUtils {

    public static void main(String[] args) {
        // 常规闭合区间的字符串
        String rangeString1 = "[1.2,3.2]";
        // 常规开放区间的字符串
        String rangeString2 = "(1.2,2.3)";
        // 带星号的区间表示
        String rangeString3 = "[1.2.3,*)";
        // 带星号的区间表示
        String rangeString4 = "(*,2.3)";

        Range<Double> range1 = parseRangeString(rangeString1);
        Range<Double> range2 = parseRangeString(rangeString2);
        Range<Double> range3 = parseRangeString(rangeString3);
        Range<Double> range4 = parseRangeString(rangeString4);

        System.out.println("解析后的区间1：" + range1);
        System.out.println("解析后的区间2：" + range2);
        System.out.println("解析后的区间3：" + range3);
        System.out.println("解析后的区间4：" + range4);
    }

    public static Range<Double> parseRangeString(String rangeString) {
        rangeString = rangeString.trim();
        if (rangeString.matches("\\[.*\\]")) {
            return parseClosedRange(rangeString);
        } else if (rangeString.matches("\\(.*\\)")) {
            return parseOpenRange(rangeString);
        } else if (rangeString.matches("\\[.*\\,\\*\\)")) {
            return parseUnboundedUpperRange(rangeString);
        } else {
            throw new IllegalArgumentException("无法解析区间字符串: " + rangeString);
        }
    }

    private static Range<Double> parseClosedRange(String rangeString) {
        String trimmedString = rangeString.substring(1, rangeString.length() - 1);
        String[] bounds = trimmedString.split(",");
        double lowerBound = parseBound(bounds[0]);
        double upperBound = parseBound(bounds[1]);
        return Range.closed(lowerBound, upperBound);
    }

    private static Range<Double> parseOpenRange(String rangeString) {
        String trimmedString = rangeString.substring(1, rangeString.length() - 1);
        String[] bounds = trimmedString.split(",");
        double lowerBound = parseBound(bounds[0]);
        double upperBound = parseBound(bounds[1]);
        return Range.open(lowerBound, upperBound);
    }

    private static Range<Double> parseUnboundedUpperRange(String rangeString) {
        String lowerBoundString = rangeString.substring(1, rangeString.indexOf(','));
        double lowerBound = parseBound(lowerBoundString);
        return Range.atLeast(lowerBound);
    }

    private static double parseBound(String boundString) {
        if ("*".equals(boundString)) {
            return Double.NEGATIVE_INFINITY;
        }
        return Double.parseDouble(boundString);
    }
}





