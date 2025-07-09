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
import com.folib.domain.gitls.command.LfsDownloadBaseCommand;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.providers.io.RepositoryPath;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactResolutionService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;

import java.io.IOException;

public class LocalDownloadCommand extends LfsDownloadBaseCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalDownloadCommand.class);

    protected ArtifactResolutionService artifactResolutionService;
    public LocalDownloadCommand(ConfigurationManager configurationManager, ArtifactRepository artifactRepository,ArtifactResolutionService artifactResolutionServic) {
       super(configurationManager, artifactRepository);
       this.artifactResolutionService = artifactResolutionServic;
    }

    public ResponseEntity<?> download(String storageId, String repositoryId, String oid, String authHeader) throws IOException {
        String oidPath = GitLfsHelper.getOidPath(oid);
        ResponseEntity<?> response = verifyCanDownload(oid, storageId,repositoryId, oidPath, true);
        if (response == null) {
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, oidPath);
            //(String oid, String storageId, String repositoryId, String oidPath, String authHeader, Long size
            response = createLfsDownloadResponse(oid, storageId,repositoryId, oidPath, authHeader, repositoryPath.getArtifactEntry().getSizeInBytes());
        }
        return response;
    }
}
