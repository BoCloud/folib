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

import com.folib.domain.gitls.model.GitLfsLocksVerification;
import com.folib.domain.gitls.model.GitLfsLock;
import com.folib.domain.gitls.model.GitLfsLocksVerificationList;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.entity.GitLfsLockEntity;
import com.folib.services.GitLfsLocalLockService;
import com.folib.users.userdetails.SpringSecurityUser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;
import java.util.stream.Collectors;

public class LocalListLockVerificationCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalListLockVerificationCommand.class);
    private GitLfsLocalLockService gitLfsLocalLockService;

    public LocalListLockVerificationCommand(GitLfsLocalLockService gitLfsLocalLockService) {
        this.gitLfsLocalLockService = gitLfsLocalLockService;
    }

    public GitLfsLocksVerificationList listLocksForVerification(String storageId, String repositoryId, GitLfsLocksVerification locksVerificationRequest) {
        List<GitLfsLockEntity> entities = gitLfsLocalLockService.listLocks(storageId, repositoryId, null, null, locksVerificationRequest.getCursor(), locksVerificationRequest.getLimit(), locksVerificationRequest.getRef().getName());
        List<GitLfsLock> locks = entities.stream().map(GitLfsHelper::readLockFromArtifact).collect(Collectors.toList());
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser user = (SpringSecurityUser) authentication.getPrincipal();
        String currentUser = user.getUsername();
        GitLfsLocksVerificationList verificationList = new GitLfsLocksVerificationList();
        String nextCursor = getNextCursor(locksVerificationRequest.getCursor(), locksVerificationRequest.getLimit(), locks);
        verificationList.setNextCursor(nextCursor);
        locks.forEach(lock -> {
            if (currentUser.equals(lock.getOwner().getName())) {
                verificationList.getOurs().add(lock);
            } else {
                verificationList.getTheirs().add(lock);
            }
        });
        return verificationList;
    }

    protected static String getNextCursor(int cursor, int limit, List<GitLfsLock> locks) {
        String nextCursor = null;
        if (limit > 0 && !locks.isEmpty() && locks.size() == limit) {
            cursor += locks.size();
            nextCursor = String.valueOf(cursor);
        }
        return nextCursor;
    }
}

