package com.veadan.folib.util;

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
