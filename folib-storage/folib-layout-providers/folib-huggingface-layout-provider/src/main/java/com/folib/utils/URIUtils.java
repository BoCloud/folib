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


import com.google.common.base.Charsets;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.SetMultimap;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.BitSet;

import com.folib.common.encoding.URI;
import lombok.Generated;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.net.URLCodec;
import org.apache.http.HttpException;
import org.apache.http.util.EncodingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class URIUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(URIUtils.class);

    static final String MATRIX_PARAM_SEPARATOR = ";";

    @Generated
    private URIUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static SetMultimap<String, String> getMatrixParamProperties(String requestURI) {
        LinkedHashMultimap linkedHashMultimap = LinkedHashMultimap.create();
        int matrixParamStart = requestURI.indexOf(";");
        if (matrixParamStart > 0) {
            int matrixParamSubstringEnd = requestURI.length();
            String matrixParamsSubstring = requestURI.substring(matrixParamStart,
                    (matrixParamSubstringEnd < 0) ? requestURI.length() : matrixParamSubstringEnd);
            matrixParamStart = 0;
            do {
                int matrixParamEnd = matrixParamsSubstring.indexOf(";", matrixParamStart + 1);
                if (matrixParamEnd < 0) {
                    matrixParamEnd = matrixParamsSubstring.length();
                }
                String param = matrixParamsSubstring.substring(matrixParamStart + 1, matrixParamEnd);
                int equals = param.indexOf('=');
                if (equals > 0) {
                    String key = param.substring(0, equals);
                    String value = param.substring(equals + 1);
                    try {
                        value = URLDecoder.decode(value, "UTF-8");
                    } catch (UnsupportedEncodingException e) {
                        log.warn("Encoding not supported: {}. Using original value", e.getMessage());
                    }
                    linkedHashMultimap.put(key, value);
                } else if (param.length() > 0) {
                    linkedHashMultimap.put(param, "");
                }
                matrixParamStart = matrixParamEnd;
            } while (matrixParamStart > 0 && matrixParamStart < matrixParamsSubstring.length());
        }
        return (SetMultimap<String, String>)linkedHashMultimap;
    }

    public static String encodeQuery(String unescaped) throws HttpException {
        return encodeQuery(unescaped, "UTF-8");
    }

    public static String encodeQuery(String unescaped, String charset) throws HttpException {
        return encode(unescaped, URI.allowed_query, charset);
    }

    public static String encode(String unescaped, BitSet allowed, String charset) throws HttpException {
        byte[] rawdata = URLCodec.encodeUrl(allowed,
                EncodingUtils.getBytes(unescaped, charset));
        return EncodingUtils.getAsciiString(rawdata);
    }

    public static String decode(String escaped) throws HttpException {
        try {
            byte[] rawdata = URLCodec.decodeUrl(EncodingUtils.getAsciiBytes(escaped));
            return EncodingUtils.getString(rawdata, Charsets.UTF_8.name());
        } catch (DecoderException e) {
            throw new HttpException(e.getMessage());
        }
    }
}

