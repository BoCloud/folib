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
package com.folib.promotion;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import lombok.Data;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.Callable;

@Data
public class ArtifactDoStoreTask implements Callable<String> {

    private InputStream fileIs;
    private String path;
    private String storageId;
    private String repostoryId;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;

    public ArtifactDoStoreTask(InputStream fileIs, String path, String storageId, String repostoryId, RepositoryPathResolver repositoryPathResolver, ArtifactManagementService artifactManagementService) {
        this.fileIs = fileIs;
        this.path = path;
        this.storageId = storageId;
        this.repostoryId = repostoryId;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
    }

    @Override
    public String call() {
        String rs = "";
        try (InputStream is = getFileIs()) {
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repostoryId, path);
            artifactManagementService.store(destPath, is);
        } catch (IOException e) {
            e.printStackTrace();
            rs = e.getMessage();
        }
        return rs;
    }
}
