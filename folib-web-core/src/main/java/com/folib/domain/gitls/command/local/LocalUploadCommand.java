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
package com.folib.domain.gitls.command.local;

import com.folib.configuration.ConfigurationManager;
import com.folib.domain.gitls.command.LfsBaseUploadCommand;

import com.folib.domain.gitls.model.GitLfsJson;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.repositories.ArtifactRepository;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

public class LocalUploadCommand extends LfsBaseUploadCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalUploadCommand.class);

    public LocalUploadCommand(ArtifactRepository artifactRepository, ConfigurationManager configurationManager) {
        super(artifactRepository, configurationManager);
    }

    public ResponseEntity<?> upload(String storageId, String repositoryId, GitLfsJson lfsJson, String authHeader) {
        String uploadPath = GitLfsHelper.getOidPath(lfsJson.getOid());

        GitLfsJson uploadResponse = createLfsUploadJson(lfsJson, authHeader, storageId, repositoryId, uploadPath);
        return buildUploadResponse(storageId, repositoryId, uploadPath, uploadResponse);
    }

    private ResponseEntity<?> buildUploadResponse(String storageId, String repositoryId, String uploadPath, GitLfsJson uploadResponse) {

        if (StringUtils.isBlank(uploadResponse.getUploadLink())) {
            log.debug("oid in Path {}/{}:{} already exists - no need to re-upload", storageId, repositoryId, uploadPath);
            return ResponseEntity.status(200).body(uploadResponse);
        } else {
            return ResponseEntity.status(202).body(uploadResponse);
        }
    }
}

