package com.veadan.folib.components.auth;

import com.google.common.collect.Sets;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.security.vote.ExtendedAuthoritiesVoter;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * @author leipenghui
 * @date 2024/11/26
 **/
@Slf4j
@Component
public class AuthComponent {

    @Autowired
    private ExtendedAuthoritiesVoter extendedAuthoritiesVoter;

    public boolean validatePrivilegesSplitPath(Repository repository, RepositoryPath repositoryPath, String authority) throws IOException {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        String relativePath = RepositoryFiles.relativizePath(repositoryPath);
        String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        String prefix = String.format("/storages/%s/%s/", storageId, repositoryId);
        relativePath = prefix + relativePath;
        Collection<String> storageAuthorities = extendedAuthoritiesVoter.getExtendedAuthorities(authentication, storageId, repositoryId, relativePath, true);
        return storageAuthorities.stream().anyMatch(item -> item.equals(authority));
    }

    public boolean validatePrivileges(Repository repository, RepositoryPath repositoryPath, String authority) throws IOException {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        String relativePath = RepositoryFiles.relativizePath(repositoryPath);
        String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        String prefix = String.format("/storages/%s/%s/", storageId, repositoryId);
        relativePath = prefix + relativePath;
        Collection<String> storageAuthorities = extendedAuthoritiesVoter.getExtendedAuthorities(authentication, storageId, repositoryId, relativePath);
        return storageAuthorities.stream().anyMatch(item -> item.equals(authority));
    }

    public Set<String> getPrivileges(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath)) {
            return Collections.emptySet();
        }
        try {
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            if (Objects.isNull(authentication)) {
                return Collections.emptySet();
            }
            Repository repository = repositoryPath.getRepository();
            String relativePath = RepositoryFiles.relativizePath(repositoryPath);
            String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
            String prefix = String.format("/storages/%s/%s/", storageId, repositoryId);
            relativePath = prefix + relativePath;
            return Sets.newLinkedHashSet(extendedAuthoritiesVoter.getExtendedAuthorities(authentication, storageId, repositoryId, relativePath));
        } catch (Exception ex) {
            log.error("Get privileges repositoryPath [{}] error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
        return Collections.emptySet();
    }

}
