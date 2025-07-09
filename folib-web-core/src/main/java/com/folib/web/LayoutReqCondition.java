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
package com.folib.web;

import com.folib.configuration.StoragesConfigurationManager;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CacheUtil;
import org.springframework.web.servlet.mvc.condition.AbstractRequestCondition;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Collection;
import java.util.Collections;

import static com.folib.web.Constants.ARTIFACT_ROOT_PATH;

public class LayoutReqCondition extends AbstractRequestCondition<ExposableRequestCondition> {

    private static final String ARTIFACT_COPY_PATH = ARTIFACT_ROOT_PATH + "/copy";

    protected final String layout;
    protected final StoragesConfigurationManager configurationManager;

    public LayoutReqCondition(StoragesConfigurationManager configurationManager, String layout) {
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
}
