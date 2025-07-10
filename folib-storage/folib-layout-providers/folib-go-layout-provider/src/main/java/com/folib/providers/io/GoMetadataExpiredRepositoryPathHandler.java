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
package com.folib.providers.io;

import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;

/**
 * @author veadan
 * @date 1/9/2024 16:51
 */
@Component
public class GoMetadataExpiredRepositoryPathHandler implements GoExpiredRepositoryPathHandler{
    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;
    @Override
    public boolean supports(final RepositoryPath repositoryPath)
            throws IOException
    {
        if (repositoryPath == null)
        {
            return false;
        }

        if (!RepositoryFiles.isMetadata(repositoryPath))
        {
            return false;
        }

        Repository repository = repositoryPath.getRepository();
        return ((RepositoryData)repository).getRemoteRepository() != null;
    }
    @Override
    public void handleExpiration(RepositoryPath repositoryPath) throws IOException {
        proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
    }
}
