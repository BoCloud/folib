package com.veadan.folib.web;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.StoragesConfigurationManager;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CacheUtil;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.servlet.mvc.condition.AbstractRequestCondition;

import javax.servlet.http.HttpServletRequest;
import java.util.Collection;
import java.util.Collections;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;
import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATHS;

public class LayoutRequestCondition extends AbstractRequestCondition<ExposableRequestCondition> {

    private static final String ARTIFACT_COPY_PATH = ARTIFACT_ROOT_PATH + "/copy";

    protected final String layout;
    protected final StoragesConfigurationManager configurationManager;

    public LayoutRequestCondition(StoragesConfigurationManager configurationManager, String layout) {
        this.layout = layout;
        this.configurationManager = configurationManager;
    }

    @Override
    public ExposableRequestCondition combine(ExposableRequestCondition other) {
        return other;
    }

    @Override
    public ExposableRequestCondition getMatchingCondition(HttpServletRequest request) {
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

        if (servletPath.startsWith(ARTIFACT_COPY_PATH)) {
            return getPathCopyCondition(request);
        }

        String finalServletPath = servletPath;
        if (ARTIFACT_ROOT_PATHS.stream().anyMatch(finalServletPath::startsWith)) {
            ExposableRequestCondition exposableRequestCondition = getStorageAndRepositoryCondition(request, servletPath);
            return exposableRequestCondition;
        }

        return null;
    }


    private ExposableRequestCondition getPathCopyCondition(HttpServletRequest request) {
        String storageId = request.getParameter("srcStorageId");
        String repositoryId = request.getParameter("srcRepositoryId");

        if (storageId == null || repositoryId == null) {
            return null;
        }
        return getStorageAndRepositoryCondition(storageId, repositoryId);
    }

    private ExposableRequestCondition getStorageAndRepositoryCondition(HttpServletRequest request, String servletPath) {
        String[] pathParts = servletPath.split("/");
        if (servletPath.startsWith(ARTIFACT_ROOT_PATH)) {
            if (pathParts.length < 4) {
                return null;
            }
            request.setAttribute("storageId", pathParts[2]);
            request.setAttribute("repositoryId", pathParts[3]);
            return getStorageAndRepositoryCondition(pathParts[2], pathParts[3]);
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
                    return getStorageAndRepositoryCondition(storageId, repositoryId);
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
            return getStorageAndRepositoryCondition(storageId, repositoryId);
        }
        return null;
    }

    private ExposableRequestCondition getStorageAndRepositoryCondition(String storageId, String repositoryId) {
        String key = String.format("%s:%s", storageId, repositoryId);
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        Repository repository = cacheUtil.get(key);
        if (repository == null) {
            Storage storage = configurationManager.getStorage(storageId);
            if (storage == null) {
                return null;
            }
            repository = storage.getRepository(repositoryId);
            if (repository == null) {
                return new RepositoryNotFoundRequestCondition(repositoryId);
            }
            cacheUtil.put(key, repository);
        }
        if (!layout.equals(repository.getLayout())) {
            //log.warn("layout not match, request layout:{}, repository layout:{}", layout, repository.getLayout());
            return null;
        }
        return new RepositoryRequestCondition(repository);
    }

    @Override
    public int compareTo(ExposableRequestCondition other, HttpServletRequest request) {
        return 1;
    }

    @Override
    protected Collection<?> getContent() {
        return Collections.singleton(layout);
    }

    @Override
    protected String getToStringInfix() {
        return layout;
    }

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
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
}
