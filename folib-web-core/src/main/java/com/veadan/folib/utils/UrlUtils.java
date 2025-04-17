package com.veadan.folib.utils;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BrowseController;
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

    public static HttpServletRequest getRequest() {
        try {
            return ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        } catch (Exception ignore) {

        }
        return null;
    }

    public static String getCurrentStorageId() {
        return getSubPath(getRequestUri(), 2);
    }

    public static String getCurrentRepositoryId() {
        return getSubPath(getRequestUri(), 3);
    }
    
    public static String getCurrentStorageIdAndRepositoryId() {
        HttpServletRequest request = getRequest();
        String servletPath = request.getServletPath();
        // 使用直接的检查而不是Optional
        if (servletPath == null) {
            servletPath = request.getPathInfo();
        } else {
            String trimmedPath = servletPath.trim();
            if (trimmedPath.isEmpty()) {
                servletPath = request.getPathInfo();
            }
        }
        if (servletPath.startsWith(Constants.ARTIFACT_COPY_PATH)) {
            String storageId = request.getParameter("srcStorageId");
            String repositoryId = request.getParameter("srcRepositoryId");
            if (storageId == null || repositoryId == null) {
                return null;
            }
            return ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId);
        }
        String finalServletPath = servletPath;
        if (ARTIFACT_ROOT_PATHS.stream().anyMatch(finalServletPath::startsWith)) {
            String[] pathParts = servletPath.split("/");
            if (servletPath.startsWith(ARTIFACT_ROOT_PATH)) {
                if (pathParts.length < 4) {
                    return null;
                }
                request.setAttribute("storageId", pathParts[2]);
                request.setAttribute("repositoryId", pathParts[3]);
                return getStorageAndRepositoryId(pathParts[2], pathParts[3]);
            }
            if (pathParts.length < 3) {
                return null;
            }
            for (String apiRootPath : Constants.ARTIFACT_API_ROOT_PATHS) {
                if (servletPath.startsWith(apiRootPath)) {
                    String path = servletPath.replace(apiRootPath, "");
                    String[] paths = path.split("/");
                    //获取对应的存储空间
                    String repositoryId = paths[0];
                    String storageId = getDefaultStorageId(repositoryId);
                    if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                        request.setAttribute("storageId", storageId);
                        request.setAttribute("repositoryId", repositoryId);
                        return getStorageAndRepositoryId(storageId, repositoryId);
                    }
                    return null;
                }
            }
            //获取对应的存储空间
            String repositoryId = pathParts[2];
            String storageId = getDefaultStorageId(repositoryId);
            if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                request.setAttribute("storageId", storageId);
                request.setAttribute("repositoryId", repositoryId);
                return getStorageAndRepositoryId(storageId, repositoryId);
            }
            return null;
        }
        if (servletPath.startsWith(BrowseController.ROOT_CONTEXT)) {
            String[] pathParts = StringUtils.removeStart(servletPath, BrowseController.ROOT_CONTEXT + GlobalConstants.SEPARATOR).split("/");
            request.setAttribute("storageId", pathParts[0]);
            request.setAttribute("repositoryId", pathParts[1]);
            return getStorageAndRepositoryId(pathParts[0], pathParts[1]);
        }
        return null;
    }

    private static String getStorageAndRepositoryId(String storageId, String repositoryId) {
        String key = String.format("%s:%s", storageId, repositoryId);
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        Repository repository = cacheUtil.get(key);
        if (repository == null) {
            ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
            Storage storage = configurationManager.getStorage(storageId);
            if (storage == null) {
                return null;
            }
            repository = storage.getRepository(repositoryId);
            if (repository == null) {
                return null;
            }
            cacheUtil.put(key, repository);
        }
        return ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId);
    }

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public static String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
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
    
    public static String addQuery(String urlStr, String key, String value) {
        final StringBuilder builder = new StringBuilder(urlStr);
        if (!urlStr.contains("?")) {
            builder.append("?");
        }
        if (!builder.toString().endsWith("?")) {
            builder.append("&");
        }
        
        builder.append(key).append("=").append(value);
        return builder.toString();
    }

}
