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
package com.folib.security.authentication.suppliers;

import com.folib.configuration.ConfigurationManager;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CacheUtil;
import org.springframework.core.annotation.Order;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Objects;

import static com.folib.web.Constants.ARTIFACT_ROOT_PATH;

@Order(2)
public abstract class LayoutAuthenticationSupplier
        implements AuthenticationSupplier {

    @Inject
    private ConfigurationManager configurationManager;

    private String layoutAlias;

    public LayoutAuthenticationSupplier(String layoutAlias) {
        this.layoutAlias = layoutAlias;
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request) {
        String uri = request.getRequestURI();
        if (!uri.startsWith(ARTIFACT_ROOT_PATH)) {
            return false;
        }

        String[] pathParts = uri.split("/");
        if (pathParts.length < 4) {
            return false;
        }

        String storageId = pathParts[2];
        String repositoryId = pathParts[3];
        if (storageId == null || repositoryId == null) {
            return false;
        }
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            if (storage == null) {
                return false;
            }
            repository = storage.getRepository(repositoryId);
            if (repository == null) {
                return false;
            }
            cacheUtil.put(key, repository);
        }
        return layoutAlias.equals(repository.getLayout());
    }
}
