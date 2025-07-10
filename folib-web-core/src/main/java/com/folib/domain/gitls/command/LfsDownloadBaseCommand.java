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
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.repositories.ArtifactRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;


public abstract class LfsDownloadBaseCommand {

    private static final Logger log = LoggerFactory.getLogger(LfsDownloadBaseCommand.class);

    protected ConfigurationManager configurationManager;
    private ArtifactRepository artifactRepository;

    protected LfsDownloadBaseCommand(ConfigurationManager configurationManager, ArtifactRepository artifactRepository) {
        this.configurationManager = configurationManager;
        this.artifactRepository = artifactRepository;
    }


    protected ResponseEntity<?> verifyCanDownload(String oid, String storageId, String repositoryId, String oidPath, boolean checkExistence) {
        if (checkExistence && !this.artifactRepository.artifactExists(storageId, repositoryId, oidPath)) {
            return notExistResponse(oid);
        } else {
            return null;
        }
    }


    protected ResponseEntity<?> createLfsDownloadResponse(String oid, String storageId, String repositoryId, String oidPath, String authHeader, Long size) {
        String baserUrl = configurationManager.getConfiguration().getBaseUrl();
        baserUrl = baserUrl.endsWith("/") ? baserUrl.substring(0, baserUrl.length() - 1) : baserUrl;
        return ResponseEntity.ok(GitLfsHelper.createLfsDownloadJson(oid, storageId, repositoryId, oidPath, authHeader, size, baserUrl));
    }

    protected static ResponseEntity<?> notExistResponse(String oid) {
        return ResponseEntity.status(404).body("oid " + oid + " doesn't exist.");
    }

    //protected boolean isPathExists(String repoKey, String oidPath) {
    //    try {
    //        return this.repositoryService.exists(repoKey, oidPath);
    //    } catch (PackageException e) {
    //        log.debug("Unable to verify existence of {}:{}, fallback to full verification", new Object[] { repoKey, oidPath, e });
    //        return false;
    //    }
    //}
    //
    //protected Response handleGetPackageException(String repoKey, String oid, String oidPath, PackageException e) {
    //    Response response;
    //    if (e.getStatus() == 403 || e.getStatus() == 401) {
    //        response = returnNoReadPermissionsResponse(oid, repoKey, oidPath);
    //    } else {
    //        log.debug("Unable to resolve artifact", (Throwable)e);
    //        response = notExistResponse(oid);
    //    }
    //    return response;
    //}
}
