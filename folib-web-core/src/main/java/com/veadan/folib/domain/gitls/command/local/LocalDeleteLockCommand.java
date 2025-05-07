package com.veadan.folib.domain.gitls.command.local;



import com.veadan.folib.domain.gitls.model.GitLfsDeleteLock;
import com.veadan.folib.domain.gitls.model.GitLfsLock;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.entity.GitLfsLockEntity;

import com.veadan.folib.services.GitLfsLocalLockService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LocalDeleteLockCommand  {

    private static final Logger log = LoggerFactory.getLogger(LocalDeleteLockCommand.class);

    private GitLfsLocalLockService gitLfsLocalLockService;

    public LocalDeleteLockCommand(GitLfsLocalLockService gitLfsLocalLockService) {
      this.gitLfsLocalLockService = gitLfsLocalLockService;
    }

    public  GitLfsLock.Root lfsDeleteLock(String storageId, String repositoryId, GitLfsDeleteLock deleteLockRequest, String lockId)  {
        GitLfsLockEntity entity = new GitLfsLockEntity()
                .setStorageId(storageId)
                .setRepositoryId(repositoryId).setId(lockId).setRef(deleteLockRequest.getRef().getName());
        entity  =  gitLfsLocalLockService.deleteLock(entity
                .setId(lockId)
                .setStorageId(storageId)
                .setRepositoryId(repositoryId));
        return deleteLockIfNeeded(storageId,repositoryId, entity);
    }

    private GitLfsLock.Root deleteLockIfNeeded(String storageId, String repositoryId,  GitLfsLockEntity entity ) {
        GitLfsLock gitLfsLock = GitLfsHelper.readLockFromArtifact(entity);

        return new GitLfsLock.Root(gitLfsLock);
    }
}

