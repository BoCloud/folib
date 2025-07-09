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

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

public class UriUtils {
    public UriUtils() {
    }

    public static String encode(String content, String enc) throws UnsupportedEncodingException {
        content = content.replace("//", "/");
        content = URLEncoder.encode(content, enc);
        content = content.replaceAll("%2F", "/");
        content = content.replaceAll("\\+", "%20");
        return content;
    }

    public static String encode(String content) throws UnsupportedEncodingException {
        content = content.replace("//", "/");
        content = URLEncoder.encode(content, StandardCharsets.UTF_8.toString());
        content = content.replaceAll("%2F", "/");
        content = content.replaceAll("\\+", "%20");
        return content;
    }

    public static String decode(String content) throws UnsupportedEncodingException {
        content = content.replaceAll("\\+", "%2B");
        content = URLDecoder.decode(content, StandardCharsets.UTF_8.toString());
        return content;
    }
}
