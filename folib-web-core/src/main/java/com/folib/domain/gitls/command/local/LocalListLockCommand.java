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

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import cn.hutool.core.util.StrUtil;
import com.folib.domain.gitls.model.GitLfsLockList;
import com.folib.domain.gitls.model.GitLfsLock;
import com.folib.domain.gitls.utils.GitLfsHelper;
import com.folib.entity.GitLfsLockEntity;
import com.folib.services.GitLfsLocalLockService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LocalListLockCommand {

    private static final Logger log = LoggerFactory.getLogger(LocalListLockCommand.class);

    private GitLfsLocalLockService gitLfsLocalLockService;

    public LocalListLockCommand(GitLfsLocalLockService gitLfsLocalLockService) {
        this.gitLfsLocalLockService = gitLfsLocalLockService;

    }

    public GitLfsLockList listLocks(String storageId, String repositoryId, String path, String id, int cursor, int limit, String refSpec) {
        SearchParams searchParams = getSearchParams(path, id, refSpec);
        List<GitLfsLock> locks = new LinkedList<>();

        locks = searchLocks(storageId, repositoryId, searchParams.pathSearch(), searchParams.fileName(), searchParams.lockId(), cursor, limit);

        String nextCursor = GitLfsHelper.getNextCursor(cursor, limit, locks);
        GitLfsLockList gitLfsLockList = new GitLfsLockList(locks, nextCursor);
        return gitLfsLockList;
    }


    protected static SearchParams getSearchParams(String path, String id, String refSpec) {
        String pathSearch, fileName = null;
        if (StrUtil.isNotEmpty(path)) {
            if (StrUtil.isEmpty(refSpec)) {
                pathSearch = path;
            } else {
                pathSearch = path;
                fileName = refSpec;
            }
        } else {
            if (StrUtil.isNotEmpty(id)) {
                return new SearchParams(null, null, id);
            }
            if (StrUtil.isNotEmpty(refSpec)) {
                pathSearch = null;
                fileName = refSpec;
            } else {
                pathSearch = null;
            }
        }
        return new SearchParams(pathSearch, fileName, null);
    }


    protected List<GitLfsLock> searchLocks(String storageId, String repositoryId, String pathSearch, String fileName, String lockId, int cursor, int limit) {
        List<GitLfsLockEntity> locksEntity = gitLfsLocalLockService.listLocks(storageId, repositoryId, pathSearch, lockId, cursor, limit, fileName);
        return locksEntity.stream().map(GitLfsHelper::readLockFromArtifact).collect(Collectors.toList());
    }


    protected static final class SearchParams {
        private final String pathSearch;

        private final String fileName;

        private final String lockId;

        protected SearchParams(String pathSearch, String fileName, String lockId) {
            this.pathSearch = pathSearch;
            this.fileName = fileName;
            this.lockId = lockId;
        }

        public String pathSearch() {
            return this.pathSearch;
        }

        public String fileName() {
            return this.fileName;
        }

        public String lockId() {
            return this.lockId;
        }

    }
}
