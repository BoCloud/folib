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
package com.folib.ext.jersey;

import org.springframework.util.StringUtils;

import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientResponseContext;
import javax.ws.rs.client.ClientResponseFilter;
import java.io.IOException;

/**
 * @author veadan
 * @date 2025/2/11
 **/
public class ContentTypeFixerFilter implements ClientResponseFilter {

    @Override
    public void filter(ClientRequestContext clientRequestContext, ClientResponseContext clientResponseContext) throws IOException {
        try {
            // 获取响应中的 Content-Type
            String contentType = clientResponseContext.getHeaderString("Content-Type");
            String key = "charset=";
            if (StringUtils.hasText(contentType) && contentType.contains(key)) {
                // 分割 Content-Type 头部，获取类型和 charset 部分
                String[] parts = contentType.split(";");
                // 如果有 charset 部分，处理多个字符集
                if (parts.length > 1) {
                    // 获取 charset 部分，可能包含多个字符集
                    String charsetPart = parts[1].trim();
                    if (!StringUtils.hasText(charsetPart)) {
                        return;
                    }
                    if (!charsetPart.contains(",")) {
                        return;
                    }
                    // 如果 charset 部分有多个字符集，选择第一个字符集
                    String[] charsets = charsetPart.split(",");
                    // 选择第一个 charset
                    String selectedCharset = charsets[0].trim();
                    selectedCharset = selectedCharset.replace(key, "");
                    // 只保留一个字符集，修正 Content-Type
                    String fixedContentType = parts[0].trim() + "; charset=" + selectedCharset;
                    clientResponseContext.getHeaders().putSingle("Content-Type", fixedContentType);
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
