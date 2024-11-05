package com.veadan.folib.web;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.veadan.folib.configuration.StoragesConfigurationManager;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CacheUtil;
import org.springframework.web.servlet.mvc.condition.AbstractRequestCondition;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.TimeUnit;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;

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

        if (servletPath.startsWith(ARTIFACT_ROOT_PATH)) {
            return getStorageAndRepositoryCondition(servletPath);
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

    private ExposableRequestCondition getStorageAndRepositoryCondition(String servletPath) {
        String[] pathParts = servletPath.split("/");

        if (pathParts.length < 4) {
            return null;
        }

        return getStorageAndRepositoryCondition(pathParts[2], pathParts[3]);
    }

    private ExposableRequestCondition getStorageAndRepositoryCondition(String storageId, String repositoryId) {
        String key = String.format("%s:%s", storageId, repositoryId);
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        Repository repository = cacheUtil.get(key);
        if (repository == null) {
            repository = configurationManager.getRepository(storageId, repositoryId);
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
}
