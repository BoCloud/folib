package com.veadan.folib.domain.gitls.command.local;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import cn.hutool.core.util.StrUtil;
import com.veadan.folib.domain.gitls.model.GitLfsLockList;
import com.veadan.folib.domain.gitls.model.GitLfsLock;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.entity.GitLfsLockEntity;
import com.veadan.folib.services.GitLfsLocalLockService;
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
