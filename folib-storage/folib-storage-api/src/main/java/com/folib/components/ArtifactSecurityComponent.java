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
package com.folib.components;

import com.google.common.collect.Lists;
import com.folib.authorization.dto.Role;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ProductTypeEnum;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.AccessModelData;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.AnonymousAccessModel;
import com.folib.users.security.AuthoritiesProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.util.UserUtils;
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
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 * @date 2024/11/27
 **/
@Slf4j
@Component
public class ArtifactSecurityComponent {

    public static final String MANIFESTS = "manifest/";

    public static final String BLOBS = "blobs/";

    @Autowired
    @Lazy
    private AuthoritiesProvider authoritiesProvider;

    public boolean validatePrivileges(RepositoryPath repositoryPath, String authority) {
        try {
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            String threadName = Thread.currentThread().getName();
            log.debug("当前线程：[{}] 用户：[{}]", threadName, UserUtils.getUsername());
            List<String> ignoreThreadNameList = GlobalConstants.IGNORE_THREAD_NAME_LIST;
            if (ignoreThreadNameList.stream().anyMatch(threadName::startsWith)) {
                return true;
            }
            if (Objects.isNull(authentication)) {
                return false;
            }
            String path = RepositoryFiles.relativizePath(repositoryPath);
            Repository repository = repositoryPath.getRepository();
            if (ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(repository.getLayout()) && (path.startsWith(MANIFESTS) || path.startsWith(BLOBS))) {
                return true;
            }
            String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
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
                if (RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())) {
                    authorities.add(Privileges.ARTIFACTS_RESOLVE);
                }
                Set<Privileges> storageAuthorities = anonymousRole.getAccessModel().getPathAuthorities(storageId, repositoryId, Lists.newArrayList(RepositoryFiles.relativizePath(repositoryPath)));
                if (!storageAuthorities.isEmpty()) {
                    authorities.addAll(storageAuthorities);
                }
                return authorities.stream().anyMatch(item -> item.getAuthority().equals(authority));
            }
            if (!(principal instanceof SpringSecurityUser)) {
                return false;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) principal;
            Collection<? extends GrantedAuthority> grantedAuthorities = authentication.getAuthorities();
            List<GrantedAuthority> authorities = Lists.newArrayList(grantedAuthorities);
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(getServerName(), storageId, repositoryId, Lists.newArrayList(RepositoryFiles.relativizePath(repositoryPath)));
            if (!storageAuthorities.isEmpty()) {
                authorities.addAll(storageAuthorities);
            }
            return authorities.stream().anyMatch(item -> item.getAuthority().equals(authority));
        } catch (Exception ex) {
            log.error("Validate privileges repositoryPath [{}] error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    public boolean anonymousValidatePrivilege(RepositoryPath repositoryPath) {
        String threadName = Thread.currentThread().getName();
        List<String> ignoreThreadNameList = GlobalConstants.IGNORE_THREAD_NAME_LIST;
        if (ignoreThreadNameList.stream().anyMatch(threadName::startsWith)) {
            return false;
        }
        String repositoryType = repositoryPath.getRepository().getType();
        if (RepositoryTypeEnum.HOSTED.getType().equals(repositoryType)) {
            return true;
        }
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return true;
        }
        Object principal = authentication.getPrincipal();
        String anonymousUser = "anonymousUser";
        if (anonymousUser.equals(principal.toString())) {
            Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
            AnonymousAccessModel anonymousAccessModel = (AnonymousAccessModel) anonymousRole.getAccessModel();
            AccessModelData accessModelData = (AccessModelData) anonymousAccessModel.getAccessModelTarget();
            return RepositoryTypeEnum.PROXY.getType().equals(repositoryType) && CollectionUtils.isNotEmpty(accessModelData.getStorageAuthorities());
        }
        return true;
    }

    public HttpServletRequest getRequest() {
        try {
            return ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        } catch (Exception ignore) {

        }
        return null;
    }

    public String getServerName() {
        try {
            HttpServletRequest request = getRequest();
            if (Objects.isNull(request)) {
                return null;
            }
            return request.getServerName();
        } catch (Exception ignore) {

        }
        return null;
    }

}
