package com.veadan.folib.domain.gitls.command.local;

import cn.hutool.core.util.StrUtil;
import com.veadan.folib.domain.gitls.model.GitLfsLocksVerification;
import com.veadan.folib.domain.gitls.constants.GitLfsConstants;
import com.veadan.folib.domain.gitls.model.GitLfsLock;
import com.veadan.folib.domain.gitls.model.GitLfsLocksVerificationList;
import com.veadan.folib.domain.gitls.utils.GitLfsHelper;
import com.veadan.folib.entity.GitLfsLockEntity;
import com.veadan.folib.services.GitLfsLocalLockService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
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

