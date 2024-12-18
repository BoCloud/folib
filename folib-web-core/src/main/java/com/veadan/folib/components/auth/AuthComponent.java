package com.veadan.folib.components.auth;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.security.vote.ExtendedAuthoritiesVoter;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.AccessModelData;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AnonymousAccessModel;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/11/26
 **/
@Slf4j
@Component
public class AuthComponent {

    @Autowired
    private ExtendedAuthoritiesVoter extendedAuthoritiesVoter;

    @Autowired
    @Lazy
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private ConfigurationManager configurationManager;

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

    public boolean validateStoragePrivileges(String storageId, String authority) throws IOException {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        String prefix = String.format("/storages/%s/", storageId);
        Collection<String> storageAuthorities = extendedAuthoritiesVoter.getExtendedAuthorities(authentication, storageId, "", prefix);
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

    public Set<String> getAllPrivileges(String storageId, String repositoryId) {
        return getAllPrivileges(storageId, repositoryId, Collections.emptyList());
    }
    public Set<String> getAllPrivileges(String storageId, String repositoryId, List<String> paths) {
        try {
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            if (Objects.isNull(authentication)) {
                return Collections.emptySet();
            }
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                return Collections.emptySet();
            }
            Repository repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                return Collections.emptySet();
            }
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                List<String> storageAndRepositoryIds = Lists.newArrayList();
                Set<String> allPrivileges = Sets.newLinkedHashSet(), privileges;
                configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
                for (String storageAndRepositoryId : storageAndRepositoryIds) {
                    String subStorageId = ConfigurationUtils.getStorageId(storageId, storageAndRepositoryId);
                    String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                    privileges = getAllPrivileges(subStorageId, subRepositoryId, paths);
                    if (CollectionUtils.isNotEmpty(privileges)) {
                        allPrivileges.addAll(privileges);
                    }
                }
                return allPrivileges;
            }
            Object principal = authentication.getPrincipal();
            String anonymousUser = "anonymousUser";
            if (anonymousUser.equals(principal.toString())) {
                //匿名角色
                Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
                Set<Privileges> anonymousApiAuthorities = anonymousRole.getAccessModel().getApiAuthorities();
                List<GrantedAuthority> authorities = Lists.newArrayList(anonymousApiAuthorities);
                AnonymousAccessModel anonymousAccessModel = (AnonymousAccessModel) anonymousRole.getAccessModel();
                AccessModelData accessModelData = (AccessModelData) anonymousAccessModel.getAccessModelTarget();
                if (CollectionUtils.isNotEmpty(accessModelData.getStorageAuthorities())) {
                    authorities.remove(Privileges.ARTIFACTS_RESOLVE);
                }
                Set<Privileges> storageAuthorities = anonymousRole.getAccessModel().getPathAuthorities(storageId, repositoryId, paths);
                if (!storageAuthorities.isEmpty()) {
                    authorities.addAll(storageAuthorities);
                }
                return authorities.stream().map(GrantedAuthority::getAuthority).collect(Collectors.toSet());
            }
            if (!(principal instanceof SpringSecurityUser)) {
                return Collections.emptySet();
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) principal;
            Collection<? extends GrantedAuthority> grantedAuthorities = authentication.getAuthorities();
            List<GrantedAuthority> authorities = Lists.newArrayList(grantedAuthorities);
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(storageId, repositoryId, Collections.emptyList());
            if (!storageAuthorities.isEmpty()) {
                authorities.addAll(storageAuthorities);
            }
            return authorities.stream().map(GrantedAuthority::getAuthority).collect(Collectors.toSet());
        } catch (Exception ex) {
            log.error("Get all privileges storageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
        }
        return Collections.emptySet();
    }

}
