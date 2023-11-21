package com.veadan.folib.utils;


import com.google.common.collect.Range;

/**
 * @author leipenghui
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





