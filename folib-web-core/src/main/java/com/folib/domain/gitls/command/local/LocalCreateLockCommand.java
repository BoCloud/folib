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
