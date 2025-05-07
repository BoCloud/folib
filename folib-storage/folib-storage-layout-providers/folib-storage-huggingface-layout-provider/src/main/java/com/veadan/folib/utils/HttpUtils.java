package com.veadan.folib.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response;

import com.veadan.folib.common.encoding.URI;
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
