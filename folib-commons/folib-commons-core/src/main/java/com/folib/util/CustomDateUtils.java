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

import com.google.common.collect.Lists;
import org.apache.commons.lang3.time.DateFormatUtils;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * @author veadan
 * @date 2022/12/28
 **/
public class CustomDateUtils extends DateFormatUtils {

    private final static DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    public static List<String> getDaysBetween(int days) {
        List<String> list = Lists.newArrayList();
        for (int i = days - 1; i >= 0; i--) {
            // 当前日期 - i天
            LocalDateTime plusDate = LocalDateTime.now().plusDays(-i);
            // 将日期转换为 yyyy-MM-dd 格式字符串
            String plusDateStr = DATE_TIME_FORMATTER.format(plusDate);
            list.add(plusDateStr);
        }
        return list;
    }
}
