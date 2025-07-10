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


import com.folib.domain.gitls.model.GitLfsCreateLock;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.domain.gitls.model.GitLfsLock;
import com.folib.domain.gitls.model.GitLfsName;
import com.folib.entity.GitLfsLockEntity;

import com.folib.services.GitLfsLocalLockService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;



public class LocalCreateLockCommand {
    private static final Logger logger = LoggerFactory.getLogger(LocalCreateLockCommand.class);

    private GitLfsLocalLockService gitLfsLocalLockService;

    public LocalCreateLockCommand(GitLfsLocalLockService gitLfsLocalLockService) {
        this.gitLfsLocalLockService = gitLfsLocalLockService;
    }

    public GitLfsLock.Root createNewLock(String storageId, String repositoryId, GitLfsCreateLock createLockJson) {
        String lockPath = GitLfsHelper.getLockFilePath(createLockJson);
        if (createLockJson.getRef() != null) {
            GitLfsCreateLock repoLockRequest = new GitLfsCreateLock();
            repoLockRequest.setPath(createLockJson.getPath());
            String repoLevelLockPath = GitLfsHelper.getLockFilePath(repoLockRequest);

            GitLfsLockEntity entity = new GitLfsLockEntity();
            entity.setPath(createLockJson.getPath());
            entity.setOwner(createLockJson.getOwner());
            entity.setRef(createLockJson.getRef().getName());
            if (gitLfsLocalLockService.getOneLock(entity) != null) {
                return null;
            }
        }
        GitLfsLock gitLfsLock = createGitLfsLock(createLockJson);
        return uploadLock(storageId, repositoryId, lockPath, gitLfsLock);
    }

    private GitLfsLock.Root uploadLock(String storageId, String repositoryId, String lockPath, GitLfsLock gitLfsLock) {
        GitLfsLockEntity entity = new GitLfsLockEntity()
                .setRef(gitLfsLock.getRef().getName())
                .setLockedAt(System.currentTimeMillis())
                .setPath(gitLfsLock.getPath())
                .setRepositoryId(repositoryId)
                .setStorageId(storageId)
                .setOwner(gitLfsLock.getOwner().getName());
        entity = gitLfsLocalLockService.createNewLock(entity);
        gitLfsLock.setId(entity.getId());
        return new GitLfsLock.Root(gitLfsLock);
    }


    private GitLfsLock createGitLfsLock(GitLfsCreateLock createLockJson) {
        return GitLfsLock.builder()
                .path(createLockJson.getPath())
                .owner(new GitLfsName(createLockJson.getOwner()))
                .lockedAt(GitLfsHelper.getRFC3339FormattedDate(System.currentTimeMillis()))
                .ref(createLockJson.getRef())
                .build();
    }
}
