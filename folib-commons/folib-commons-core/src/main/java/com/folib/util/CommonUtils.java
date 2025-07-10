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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @date 2022/11/25
 **/
@Slf4j
public class CommonUtils {

    /**
     * 读出文件的最后n行
     *
     * @param path 文件
     * @param num  第几行
     * @return 读取文本文件的最后n行
     */
    public static String readLastLines(Path path, int num) {
        if (num == 0) {
            return "";
        }
        // 判断该文件是否存在，可读
        if (!Files.exists(path) || Files.isDirectory(path) || !Files.isReadable(path)) {
            return "";
        }
        List<String> list = Lists.newArrayList();
        // 行数
        int count = 0;
        RandomAccessFile rafFile = null;
        try {
            // 选择只读模式
            rafFile = new RandomAccessFile(path.toAbsolutePath().toFile(), "r");
            // 读取文件长度
            long length = rafFile.length();
            // 判断长度
            if (length == 0L) {
                return "";
            } else {
                // 因为是倒数，所以从最大值开始读起
                long pos = length - 1;
                // 当下一个大于0，则代表文章有内容
                while (pos > 0) {
                    // 开始读取
                    rafFile.seek(pos);
                    // 如果读取到\n代表是读取到一行
                    if (rafFile.readByte() == '\n') {
                        String line = rafFile.readLine();
                        if (StringUtils.isNotBlank(line)) {
                            line = new String(line.getBytes(StandardCharsets.ISO_8859_1), StandardCharsets.UTF_8);
                            // 保存结果
                            list.add(line);
                            // 行数统计，如果到达了指定的行数，就跳出循环
                            count++;
                        }
                        if (count == num) {
                            break;
                        }
                    }
                    pos--;
                }
                //next为0，代表长度为1
                if (pos == 0) {
                    rafFile.seek(0);
                    list.add(rafFile.readLine());
                }
            }
            Collections.reverse(list);
            return String.join("\n", list);
        } catch (IOException e) {
            log.error("path ：{} readLastLines ：{} error：{}", path, num, ExceptionUtils.getStackTrace(e));
            return "";
        } finally {
            if (rafFile != null) {
                try {
                    rafFile.close();
                } catch (IOException e) {
                    log.error("path ：{} readLastLines ：{} error：{}", path, num, ExceptionUtils.getStackTrace(e));
                }
            }
        }
    }

    /**
     * 获取实际异常信息
     *
     * @param e Throwable
     * @return 异常信息
     */
    public static String getRealMessage(Throwable e) {
        // 如果e不为空，则去掉外层的异常包装
        String s = "";
        String ex = "Exception:";
        while (e != null) {
            Throwable cause = e.getCause();
            if (cause == null) {
                s = e.getMessage();
                if (StringUtils.isBlank(s)) {
                    s = "";
                }
                if (s.contains(ex)) {
                    s = s.substring(s.indexOf(ex) + ex.length());
                }
                return s;
            }
            e = cause;
        }
        return "";
    }

    /**
     * 是否是需要捕获的异常信息
     *
     * @param e Throwable
     * @return true 是 false 不是
     */
    public static boolean catchException(Throwable e) {
        return catchException(getRealMessage(e));
    }

    /**
     * 是否是需要捕获的异常信息
     *
     * @param err 异常信息
     * @return true 是 false 不是
     */
    public static boolean catchException(String err) {
        String expectedErr = "Expected value mismatch for KeyColumn";
        String lockErr = "Local lock contention";
        String uniquenessErr = "violates a uniqueness constraint";
        String lockNoTimestamp = "no lock column contained our timestamp";
        List<String> errList = Lists.newArrayList(expectedErr, lockErr, uniquenessErr, lockNoTimestamp);
        return errList.stream().anyMatch(err::contains);
    }

    /**
     * 把数组所有元素排序，并按照“参数=参数值”的模式用“&”字符拼接成字符串
     *
     * @param params 需要排序并参与字符拼接的参数组
     * @return 拼接后字符串
     */
    public static String createLinkStringByGet(Map<String, String> params) {
        String preStr = "?";
        try {
            List<String> keys = new ArrayList<String>(params.keySet());
            Collections.sort(keys);
            for (int i = 0; i < keys.size(); i++) {
                String key = keys.get(i);
                String value = params.get(key);
                value = URLEncoder.encode(value, "UTF-8");
                if (i == keys.size() - 1) {
                    //拼接时，不包括最后一个&字符
                    preStr = preStr + key + "=" + value;
                } else {
                    preStr = preStr + key + "=" + value + "&";
                }
            }
        } catch (Exception ex) {
            log.error("字符拼接错误：{}", ExceptionUtils.getStackTrace(ex));
        }
        return preStr;
    }

    /**
     * 毫秒数转为UTC时间
     *
     * @param millis 毫秒数
     * @return UTC时间
     */
    public static String getConvertMillis2String(long millis) {
        // 将毫秒数转换为Instant对象
        Instant instant = Instant.ofEpochMilli(millis);
        // 转换为UTC时间
        DateTimeFormatter formatter = DateTimeFormatter.ISO_INSTANT;
        return instant.atOffset(ZoneOffset.UTC).format(formatter);
    }

    /**
     * 比较两个map是否一致
     *
     * @param map1 map1
     * @param map2 map2
     * @return true 两个map一致 false 两个map不一致
     */
    public static boolean areMapsEqual(Map<String, String> map1, Map<String, String> map2) {
        if (map1.size() != map2.size()) {
            return false;
        }
        for (Map.Entry<String, String> entry : map1.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            if (!map2.containsKey(key) || !map2.get(key).equals(value)) {
                return false;
            }
        }
        return true;
    }
}
