package com.veadan.folib.utils;

import com.veadan.folib.scanner.common.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;

/**
 * @author
 */
@Slf4j
public class UrlUtils {

    private UrlUtils() {
    }

    public static String getRequestUri() {
        HttpServletRequest servletRequest = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        return servletRequest.getRequestURI();
    }

    public static String getCurrentStorageId() {
        return getSubPath(getRequestUri(), 2);
    }

    public static String getCurrentRepositoryId() {
        return getSubPath(getRequestUri(), 3);
    }

    private static String getSubPath(String url,
                                     int index) {
        String[] args = url.split("/");
        if (args.length < index + 1) {
            return null;
        }
        return args[index];
    }


    public static String[] parsePath(String artifactPath) {
        try {
            URL url = new URL(artifactPath);
            String path = url.getPath();
            String hostUrl = url.getHost();
            String[] parts = path.split("/");
            String storageId = parts[1];
            String repositoryId = parts[2];
            return new String[]{storageId, repositoryId, hostUrl};
        } catch (Exception e) {
            // URL 格式不正确或解析失败
            throw new BusinessException(String.format("%s URL 格式不正确或解析失败", artifactPath));
        }
    }

    public static Integer getPort(String urlStr) {
        if (urlStr.startsWith("https")) {
            return 443;
        }
        try {
            final URL url = new URL(urlStr);
            return Optional.of(url.getPort()).map(p -> p < 0 ? 80 : p).get();
        } catch (MalformedURLException e) {
            log.error("解析端口错误", e);
            return null;
        }
    }

    public static String getHost(String urlStr) {
        try {
            final URL url = new URL(urlStr);
            return url.getHost();
        } catch (MalformedURLException e) {
            log.error("解析Host错误", e);
            return null;
        }
    }

}
