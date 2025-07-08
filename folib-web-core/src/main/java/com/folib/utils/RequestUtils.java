package com.folib.utils;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


public class RequestUtils {


    private static final String STORAGES_PREFIX = "/storages/";
    private static final String BROWSE_PREFIX = "/api/browse/";

    public static HttpServletRequest getCurrentHttpRequest() {

        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if (requestAttributes instanceof ServletRequestAttributes servletRequestAttributes) {
            return servletRequestAttributes.getRequest();
        }
        return null;
    }
    public static String getStorageId() {
        HttpServletRequest request = getCurrentHttpRequest();
        if (request == null) return null;
        return extractPathPart(request, 2);
    }

    public static String getRepositoryId() {
        HttpServletRequest request = getCurrentHttpRequest();
        if (request == null) return null;
        return extractPathPart(request, 3);
    }


    private static String extractPathPart(HttpServletRequest request, int index) {
        //从请求路径中通过正则提取
        String path = request.getRequestURI(); // /storages/public/releases/com/foo/bar
        String contextPath = request.getContextPath();

        if (!contextPath.isEmpty() && path.startsWith(contextPath)) {
            path = path.substring(contextPath.length());
        }
        String[] parts = path.split("/");
        if(request.getServletPath().startsWith(STORAGES_PREFIX)){
            if (parts.length > index && "storages".equals(parts[1])) {
                return parts[index];
            }
        }else if (request.getServletPath().startsWith(BROWSE_PREFIX)){
            if (parts.length > index && "api".equals(parts[1]) && "browse".equals(parts[2])) {
                return parts[index+1];
            }
        }

        return null;
    }

    public static String extractPathFromRequest() {
        HttpServletRequest request = RequestUtils.getCurrentHttpRequest();
        if (request == null) return null;
        // 如 /storages/public/releases/com/example/a.txt
        String uri = request.getRequestURI();
        // 一般是 ""，除非部署到子路径
        String contextPath = request.getContextPath();

        // 去掉 contextPath
        if (!contextPath.isEmpty() && uri.startsWith(contextPath)) {
            uri = uri.substring(contextPath.length());
        }
        // 假设你的路径总是以 /storages/{storageId}/{repositoryId}/ 开头
        // 找第4个斜杠后面的内容
        String[] parts = uri.split("/", 5);

        if(request.getServletPath().startsWith(STORAGES_PREFIX)){
            if (parts.length >= 5) {
                // 第4个斜杠之后的部分
                List<String> paths = new ArrayList<>(parts.length-4);
                for (int i = 4; i < parts.length; i++){
                    paths.add(parts[i]);
                }
                return paths.stream().collect(Collectors.joining("/"));
            }
        }else if (request.getServletPath().startsWith(BROWSE_PREFIX)){
            if (parts.length >= 6) {
                List<String> paths = new ArrayList<>(parts.length-5);
                for (int i = 5; i < parts.length; i++){
                    paths.add(parts[i]);
                }
                return paths.stream().collect(Collectors.joining("/"));
            }
        }
        return null;
    }
}
