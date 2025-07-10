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
package com.folib.domain.gitls.command;

import com.folib.configuration.ConfigurationManager;
import com.folib.domain.gitls.model.GitLfsJson;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.repositories.ArtifactRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class LfsBaseUploadCommand {
    private static final Logger log = LoggerFactory.getLogger(LfsBaseUploadCommand.class);


    private ArtifactRepository artifactRepository;

    protected ConfigurationManager configurationManager;

    protected LfsBaseUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager) {
        this.artifactRepository = artifactRepository;
        this.configurationManager = configurationManager;
    }

    protected GitLfsJson createLfsUploadJson(GitLfsJson requestJson, String authHeader, String storageId, String repositoryId, String uploadPath) {
        GitLfsJson uploadResponse = new GitLfsJson(requestJson);
        if (isPathExists(storageId, repositoryId, uploadPath)) {
            return uploadResponse;
        }
        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
        uploadResponse.setUploadLink(GitLfsHelper.getArtifactLfsUrl(baserUrl, storageId, repositoryId, uploadPath, requestJson.getOid()));
        GitLfsHelper.addAuthHeaderIfPresent(uploadResponse.getUploadHeaders(), authHeader);
        GitLfsHelper.addChecksumVerificationHeader(uploadResponse.getUploadHeaders(), requestJson.getOid());
        return uploadResponse;
    }

    protected boolean isPathExists(String storageId, String repositoryId, String oidPath) {
        return this.artifactRepository.artifactExists(storageId, repositoryId, oidPath);
    }
}

