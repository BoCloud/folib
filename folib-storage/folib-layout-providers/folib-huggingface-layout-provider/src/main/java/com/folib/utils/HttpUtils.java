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

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;
import java.util.Set;
import javax.annotation.Nonnull;

import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response;

import com.folib.common.encoding.URI;
import lombok.Generated;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.net.URLCodec;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.util.EncodingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class HttpUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(HttpUtils.class);

    private static final String DEFAULT_ENCODING = "utf-8";

    public static final String BASIC_AUTHORIZATION_HEADER = "Basic ";

    public static final String URL_SEPARATOR = "://";

    @Generated
    private HttpUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static boolean isRedirectionResponseCode(int status) {
        return (300 <= status && status <= 399);
    }

    public static boolean isSuccessfulResponseCode(int status) {
        return (200 <= status && status <= 299);
    }

    public static boolean isErrorResponseCode(int status) {
        return (400 <= status && status <= 599);
    }

    public static boolean isInSuccessOrRedirectResponseCodeRange(int status) {
        return (isSuccessfulResponseCode(status) || isRedirectionResponseCode(status));
    }

    public static boolean isInSuccessOrRedirectResponseCodeRange(Response response) {
        return (response != null && isInSuccessOrRedirectResponseCodeRange(response.getStatus()));
    }

    public static boolean isHeadRequest(String httpMethod) {
        return "HEAD".equalsIgnoreCase(httpMethod);
    }

    public static boolean isGetRequest(String httpMethod) {
        return "GET".equalsIgnoreCase(httpMethod);
    }

    public static String encodeQuery(String unescaped) {
        try {
            byte[] rawData = URLCodec.encodeUrl(URI.allowed_query, EncodingUtils.getBytes(unescaped, "UTF-8"));
            return EncodingUtils.getAsciiString(rawData);
        } catch (Exception e) {
            log.warn("Could not encode path '{}' with UTF-8 charset, returning the un-escaped value.", unescaped);
            return unescaped;
        }
    }

    public static boolean isBasicAuthHeaderPresent(@Nonnull HttpServletRequest request) {
        return getBasicAuthorization(request).isPresent();
    }

    public static Optional<String> getBasicAuthorization(@Nonnull HttpServletRequest request) {
        return Optional.<String>ofNullable(request.getHeader("Authorization"))
                .filter(value -> value.startsWith("Basic "))
                .map(value -> value.substring("Basic ".length()))
                .filter(StringUtils::isNotBlank)
                .filter(value -> !"Og==".equals(value));
    }

    public static String extractUsernameFromRequest(@Nonnull ServletRequest request) {
        String header = ((HttpServletRequest)request).getHeader("Authorization");
        if (header != null && header.startsWith("Basic ")) {
            String token;
            try {
                byte[] base64Token = header.substring(6).getBytes("utf-8");
                token = new String(Base64.decodeBase64(base64Token), "utf-8");
            } catch (UnsupportedEncodingException e) {
                log.info("the encoding is not supported");
                return "";
            }
            String username = "";
            int delim = token.indexOf(':');
            if (delim != -1) {
                username = token.substring(0, delim);
            }
            return username;
        }
        return "";
    }

    public static boolean isValidUrl(String url) {
        try {
            new URL(url);
            return url.contains("://");
        } catch (MalformedURLException e) {
            return false;
        }
    }

    public static boolean isEtagNotModified(HttpServletRequest servletRequest, String etag) {
        if (StringUtils.isBlank(etag) || servletRequest == null) {
            return false;
        }
        String requestIfNoneMatch = servletRequest.getHeader("If-None-Match");
        return etag.equals(requestIfNoneMatch);
    }

    public static String removeUrlProtocol(String url) {
        return url.replaceAll("^http[s]?", "");
    }

    public static String removeUrlProtocolIncludingSlashes(String url) {
        return url.replaceAll("^https?://", "");
    }

    public static boolean isResponseOkOrRedirectedMovedTemporarilyCode( Response response) {
        Set<Integer> acceptedStatuses = Set.of(Integer.valueOf(200), Integer.valueOf(302), Integer.valueOf(304));
        return (response != null && acceptedStatuses
                .contains(Integer.valueOf(response.getStatus())));
    }

    public static String getOverrideContextPath( HttpServletRequest httpServletRequest,  String artifactoryBaseUrl) {
        if (StringUtils.isBlank(artifactoryBaseUrl)) {
            return "/artifactory";
        }
        return "/" + PathUtils.getLastPathElement(artifactoryBaseUrl);
    }
}
