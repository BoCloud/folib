package com.veadan.folib.utils;

import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.net.URL;

/**
 * @author
 */
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


    public static void main(String[] args) {
        String sourcePath = "";
        String[] result = parsePath(sourcePath);
        if (result != null) {
            String srcStorageId = result[0];
            String srcRepostoryId = result[1];
            System.out.println("srcStorageId: " + srcStorageId);
            System.out.println("srcRepostoryId: " + srcRepostoryId);
        } else {
            System.out.println("Invalid sourcePath format");
        }
    }

    public static String[] parsePath(String artiactPath) {
        try {
            URL url = new URL(artiactPath);
            String path = url.getPath();
            String hostUrl = url.getHost();
            String[] parts = path.split("/");
            if (parts.length >= 0) {
                String storageId = parts[1];
                String repostoryId = parts[2];
                return new String[]{storageId, repostoryId, hostUrl};
            }

        } catch (Exception e) {
            // URL 格式不正确或解析失败
        }
        return null;
    }

}
