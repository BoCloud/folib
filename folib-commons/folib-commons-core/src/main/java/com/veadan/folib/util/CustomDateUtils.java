package com.veadan.folib.util;

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
