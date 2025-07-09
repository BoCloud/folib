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
package com.folib.common;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import org.apache.commons.lang3.StringUtils;

public class SafeUriDeserializer extends JsonDeserializer<URI> {
    public URI deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        JsonNode node = (JsonNode)jsonParser.getCodec().readTree(jsonParser);
        String value = node.asText();
        return create(value);
    }

    static URI create(String uri) {
        if (StringUtils.isBlank(uri))
            return URI.create("");
        try {
            URL url = new URL(uri);
            String path = url.getPath();
            String query = url.getQuery();
            String fragment = url.getRef();
            String protocol = url.getProtocol();
            String userInfo = url.getUserInfo();
            String host = url.getHost();
            int port = url.getPort();
            return new URI(protocol, userInfo, host, port, path, query, fragment);
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }
}
