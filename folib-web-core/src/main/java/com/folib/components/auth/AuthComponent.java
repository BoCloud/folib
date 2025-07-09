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
package com.folib.components.auth;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.authorization.dto.Role;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.security.vote.ExtendedAuthoritiesVoter;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.AccessModelData;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.AnonymousAccessModel;
import com.folib.users.security.AuthoritiesProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.utils.UrlUtils;
import jakarta.servlet.http.HttpServletRequest;
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
 * @author veadan
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
            boolean globalAllowAnonymous = configurationManager.getConfiguration().getAdvancedConfiguration().isAllowAnonymous();
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
                if (!globalAllowAnonymous || !repository.isAllowAnonymous()) {
                    //全局禁止匿名访问或者仓库禁止匿名访问
                    return Collections.emptySet();
                }
                //匿名角色
                Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
                Set<Privileges> anonymousApiAuthorities = anonymousRole.getAccessModel().getApiAuthorities();
                List<GrantedAuthority> authorities = Lists.newArrayList(anonymousApiAuthorities);
                AnonymousAccessModel anonymousAccessModel = (AnonymousAccessModel) anonymousRole.getAccessModel();
                AccessModelData accessModelData = (AccessModelData) anonymousAccessModel.getAccessModelTarget();
                if (CollectionUtils.isNotEmpty(accessModelData.getStorageAuthorities())) {
                    authorities.remove(Privileges.ARTIFACTS_RESOLVE);
                }
                if (RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())) {
                    authorities.add(Privileges.ARTIFACTS_RESOLVE);
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
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(getServerName(), storageId, repositoryId, Collections.emptyList());
            if (!storageAuthorities.isEmpty()) {
                authorities.addAll(storageAuthorities);
            }
            return authorities.stream().map(GrantedAuthority::getAuthority).collect(Collectors.toSet());
        } catch (Exception ex) {
            log.error("Get all privileges storageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
        }
        return Collections.emptySet();
    }

    public boolean hasAdmin() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return false;
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) o;
        if (CollectionUtils.isEmpty(userDetails.getRoles())) {
            return false;
        }
        return userDetails.getRoles().stream().anyMatch(item -> SystemRole.ADMIN.name().equals(item.getName()));
    }

    public boolean validatePathPrivileges(String storageId, String repositoryId, List<String> paths, String authority) {
        return getAllPrivileges(storageId, repositoryId, paths).contains(authority);
    }

    // 根据仓库验证 增加组合仓库过滤
    public boolean validatePathPrivileges(Repository repository, List<String> paths, String authority) {
        if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
            List<String> storageAndRepositoryIds = new LinkedList<>();
            configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
            for (String storageAndRepositoryId : storageAndRepositoryIds) {
                String subStorageId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(subStorageId, subRepositoryId);
                if (RepositoryScopeEnum.OPEN.getType().equals(subRepository.getScope()) || validatePathPrivileges(subRepository, paths, authority)) {
                    return true;
                }
            }
            return false;
        } else {
            return validatePathPrivileges(repository.getStorage().getId(), repository.getId(), paths, authority);
        }
    }

    public String getServerName() {
        try {
            HttpServletRequest request = UrlUtils.getRequest();
            if (Objects.isNull(request)) {
                return null;
            }
            return request.getServerName();
        } catch (Exception ignore) {

        }
        return null;
    }

}
