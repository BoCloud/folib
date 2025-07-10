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

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author veadan
 * @date 2022/10/19
 **/
public class FileSizeConvertUtils {

    /**
     * 文件大小转换
     *
     * @param sizeInBytes 字节大小
     * @return B、KB、MB、GB
     */
    public static String convert(Long sizeInBytes) {
        BigDecimal bigDecimal = BigDecimal.valueOf(sizeInBytes);
        String size = "";
        double kb = 1024;
        double mb = 1024 * 1024;
        double gb = 1024 * 1024 * 1024;
        double bSize = 0.1 * kb;
        double kbSize = 0.1 * mb;
        double mbSize = 0.1 * gb;
        if (sizeInBytes < bSize) {
            //如果小于0.1KB转化成B
            size = bigDecimal.setScale(2, RoundingMode.HALF_UP) + "B";
        } else if (sizeInBytes < kbSize) {
            //如果小于0.1MB转化成KB
            size = bigDecimal.divide(BigDecimal.valueOf(kb), 2, RoundingMode.HALF_UP) + "KB";
        } else if (sizeInBytes < mbSize) {
            //如果小于0.1GB转化成MB
            size = bigDecimal.divide(BigDecimal.valueOf(mb), 2, RoundingMode.HALF_UP) + "MB";
        } else {
            //其他转化成GB
            size = bigDecimal.divide(BigDecimal.valueOf(gb), 2, RoundingMode.HALF_UP) + "GB";
        }
        return size;
    }

    public static double convertBytes(long bytes, String targetUnit) {
        double convertedSize = bytes;
        switch (targetUnit) {
            case "KB":
                convertedSize /= 1024;
                break;
            case "MB":
                convertedSize /= (1024 * 1024);
                break;
            case "GB":
                convertedSize /= (1024 * 1024 * 1024);
                break;
            case "TB":
                convertedSize /= (1024L * 1024 * 1024 * 1024);
                break;
            case "PB":
                convertedSize /= (1024L * 1024 * 1024 * 1024 * 1024);
                break;
            default:
                throw new IllegalArgumentException("Unsupported target unit: " + targetUnit);
        }
        return convertedSize;
    }

    public static BigDecimal convertBytesWithDecimal(long bytes, String targetUnit) {
        BigDecimal convertedSize = BigDecimal.valueOf(bytes);
        BigDecimal divisor = BigDecimal.ONE;
        switch (targetUnit) {
            case "KB":
                divisor = BigDecimal.valueOf(1024);
                break;
            case "MB":
                divisor = BigDecimal.valueOf(1024 * 1024);
                break;
            case "GB":
                divisor = BigDecimal.valueOf(1024 * 1024 * 1024);
                break;
            case "TB":
                divisor = BigDecimal.valueOf(1024L * 1024 * 1024 * 1024);
                break;
            case "PB":
                divisor = BigDecimal.valueOf(1024L * 1024 * 1024 * 1024 * 1024);
                break;
            default:
                throw new IllegalArgumentException("Unsupported target unit: " + targetUnit);
        }
        return convertedSize.divide(divisor, 4, RoundingMode.HALF_UP);
    }

    public static long convertToBytes(long size, String unit) {
        switch (unit.toUpperCase()) {
            case "KB":
                return size * 1024;
            case "MB":
                return size * 1024 * 1024;
            case "GB":
                return size * 1024 * 1024 * 1024;
            case "TB":
                return size * 1024L * 1024 * 1024 * 1024;
            default:
                throw new IllegalArgumentException("Unsupported unit: " + unit);
        }
    }
}

