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
package com.folib.security.vote;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.folib.authorization.dto.Role;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.controllers.BrowseController;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.security.resolvepath.ResolvePathProvider;
import com.folib.security.resolvepath.ResolvePathProviderRegistry;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.AccessModelData;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.AnonymousAccessModel;
import com.folib.users.security.AuthoritiesProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.util.CacheUtil;
import com.folib.util.UriUtils;
import com.folib.utils.UrlUtils;
import jakarta.servlet.http.HttpServletRequest;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.ExpressionBasedPreInvocationAdvice;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdviceVoter;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

import static com.folib.web.Constants.*;

/**
 * @author veadan
 */
@Component
public class ExtendedAuthoritiesVoter extends PreInvocationAuthorizationAdviceVoter {

    private final Logger logger = LoggerFactory.getLogger(ExtendedAuthoritiesVoter.class);

    @Autowired
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    @Lazy
    private AuthoritiesProvider authoritiesProvider;

    @Autowired
    @Lazy
    private ConfigurationManager configurationManager;

    @Autowired
    @Lazy
    private ArtifactRepository artifactRepository;

    @Autowired
    @Lazy
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @Autowired
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;


    public ExtendedAuthoritiesVoter() {
        super(new ExpressionBasedPreInvocationAdvice());
    }

    @Override
    public int vote(Authentication authentication,
                    MethodInvocation method,
                    Collection<ConfigAttribute> attributes) {
        return super.vote(new ExtendedAuthorityAuthentication(authentication), method, attributes);
    }

    public Collection<String> getExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path, Boolean enableSplitPath) {
        ExtendedAuthorityAuthentication extendedAuth = new ExtendedAuthorityAuthentication(authentication);
        return extendedAuth.calculateExtendedAuthorities(authentication, storageId, repositoryId, path, enableSplitPath).stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
    }

    public Collection<String> getExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path) {
        ExtendedAuthorityAuthentication extendedAuth = new ExtendedAuthorityAuthentication(authentication);
        return extendedAuth.calculateExtendedAuthorities(authentication, storageId, repositoryId, path, false).stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
    }

    @SuppressWarnings("serial")
    public class ExtendedAuthorityAuthentication implements Authentication {

        private Authentication source;

        public ExtendedAuthorityAuthentication(Authentication target) {
            super();
            this.source = target;
        }

        private Authentication getSourceAuthentication() {
            return source;
        }

        private Boolean getRepositoryAllowAnonymousFromCacheOrLoad(String storageId, String repositoryId) {
            CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
            String key = String.format("%s:%s", storageId, repositoryId);
            Repository repository = cacheUtil.get(key);
            if (Objects.isNull(repository)) {
                Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
                if (Objects.isNull(storage)) {
                    return true;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    return true;
                }
                cacheUtil.put(key, repository);
            }
            return repository.isAllowAnonymous();
        }

        private Repository getRepositoryFromCacheOrLoad(String storageId, String repositoryId) {
            CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
            String key = String.format("%s:%s", storageId, repositoryId);
            Repository repository = cacheUtil.get(key);
            if (Objects.isNull(repository)) {
                Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
                if (Objects.isNull(storage)) {
                    return null;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    return null;
                }
                cacheUtil.put(key, repository);
            }
            return repository;
        }

        public Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path, Boolean enableSplitPath) {
            storageId = storageId == null ? UrlUtils.getCurrentStorageId() : storageId;
            repositoryId = repositoryId == null ? UrlUtils.getCurrentRepositoryId() : repositoryId;
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> globalAuthorities = authentication.getAuthorities();
            List<GrantedAuthority> apiAuthorities = Lists.newArrayList();
            globalAuthorities.forEach(item -> {
                apiAuthorities.add(SerializationUtils.clone(item));
            });
            final boolean result = handlerRestrictedRepository(apiAuthorities, storageId, repositoryId);
            if (result) {
                Authentication newAuthentication = new UsernamePasswordAuthenticationToken(
                        authentication.getPrincipal(),
                        authentication.getCredentials(),
                        apiAuthorities
                );
                SecurityContextHolder.getContext().setAuthentication(newAuthentication);
                return apiAuthorities;
            }
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);
            String requestUri = path == null ? parseRequestUri(UrlUtils.getRequestUri()) : path;
            Repository repository = getRepositoryFromCacheOrLoad(storageId, repositoryId);
            if (Objects.nonNull(repository)) {
                // 判断是否为组合库
                if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    // 获取所有子仓库
                    List<String> storageAndRepositoryIds = new LinkedList<>();
                    configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
                    Set<GrantedAuthority> extendedAuthorities = new HashSet<>();
                    String storeAndRepo = storageId + "/" + repositoryId + "/";
                    int index = requestUri.indexOf(storeAndRepo);
                    String relativePath = "", sourceRelativePath = "";
                    if (index != -1) {
                        relativePath = requestUri.substring(index + storeAndRepo.length());
                        sourceRelativePath = relativePath;
                    }
                    String resolvePathType = ResolvePathTypeEnum.getResolvePathType(repository.getLayout());
                    if (StringUtils.isNotBlank(resolvePathType)) {
                        ResolvePathProvider resolvePathProvider = resolvePathProviderRegistry.getProvider(resolvePathType);
                        if (Objects.nonNull(resolvePathProvider)) {
                            relativePath = resolvePathProvider.resolvePath(repository, relativePath);
                        }
                    }
                    for (String storageAndRepositoryId : storageAndRepositoryIds) {
                        String subStorageId = ConfigurationUtils.getStorageId(storageId, storageAndRepositoryId);
                        String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                        String newPath = rewriteByStoreAndRepo(requestUri, subStorageId, subRepositoryId);
                        if (StringUtils.isNotBlank(relativePath)) {
                            newPath = String.format("/storages/%s/%s/%s", subStorageId, subRepositoryId, sourceRelativePath);
                        }
                        Collection<? extends GrantedAuthority> grantedAuthorities = calculateExtendedAuthorities(authentication, subStorageId, subRepositoryId, newPath, enableSplitPath);
                        extendedAuthorities.addAll(grantedAuthorities);
                    }
                    return extendedAuthorities;
                }
                String resolvePathType = ResolvePathTypeEnum.getResolvePathType(repository.getLayout());
                if (StringUtils.isNotBlank(resolvePathType)) {
                    ResolvePathProvider resolvePathProvider = resolvePathProviderRegistry.getProvider(resolvePathType);
                    if (Objects.nonNull(resolvePathProvider)) {
                        requestUri = resolvePathProvider.resolvePath(repository, requestUri);
                    }
                }
            }
            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
                if (!configurationManagementService.getConfiguration().getAdvancedConfiguration().isAllowAnonymous()) {
                    return Collections.emptySet();
                }
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    if (Boolean.FALSE.equals(getRepositoryAllowAnonymousFromCacheOrLoad(storageId, repositoryId))) {
                        return Collections.emptySet();
                    }
                }
                Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
                Set<Privileges> anonymousApiAuthorities = anonymousRole.getAccessModel().getApiAuthorities();
                List<GrantedAuthority> authorities = new ArrayList<>(anonymousApiAuthorities);
                AnonymousAccessModel anonymousAccessModel = (AnonymousAccessModel) anonymousRole.getAccessModel();
                AccessModelData accessModelData = (AccessModelData) anonymousAccessModel.getAccessModelTarget();
                if (CollectionUtils.isNotEmpty(accessModelData.getStorageAuthorities())) {
                    authorities.remove(Privileges.ARTIFACTS_RESOLVE);
                }
                if (Objects.nonNull(repository) && RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())) {
                    authorities.add(Privileges.ARTIFACTS_RESOLVE);
                }
                List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
                if (StringUtils.isNotBlank(requestUri) && paths.stream().noneMatch(requestUri::startsWith)) {
                    return authorities;
                }
                Set<Privileges> storageAuthorities = anonymousRole.getAccessModel().getPathAuthorities(requestUri, enableSplitPath);
                if (storageAuthorities.isEmpty()) {
                    return authorities;
                }
                authorities.addAll(storageAuthorities);
                return authorities;
            } else if (!(principal instanceof SpringSecurityUser)) {
                logger.warn("Unknown authentication principal type [{}]", principal.getClass());
                return authentication.getAuthorities();
            }
            List<GrantedAuthority> extendedAuthorities = new ArrayList<>(apiAuthorities);
            if (Objects.nonNull(repository) && RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()) && !extendedAuthorities.contains(Privileges.ARTIFACTS_RESOLVE)) {
                extendedAuthorities.add(Privileges.ARTIFACTS_RESOLVE);
            }
            List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
            if (StringUtils.isNotBlank(requestUri) && paths.stream().noneMatch(requestUri::startsWith)) {
                return extendedAuthorities;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(requestUri, enableSplitPath);
            if (storageAuthorities.isEmpty()) {
                return extendedAuthorities;
            }
            extendedAuthorities.addAll(storageAuthorities);
            logger.debug("Privileges for [{}] was extended to [{}]", userDetails.getUsername(), extendedAuthorities);
            return extendedAuthorities;
        }

        @Override
        public String getName() {
            return getSourceAuthentication().getName();
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return calculateExtendedAuthorities(getSourceAuthentication(), null, null, null, false);
        }

        @Override
        public Object getCredentials() {
            return getSourceAuthentication().getCredentials();
        }

        @Override
        public Object getDetails() {
            return getSourceAuthentication().getDetails();
        }

        @Override
        public Object getPrincipal() {
            return getSourceAuthentication().getPrincipal();
        }

        @Override
        public boolean isAuthenticated() {
            return getSourceAuthentication().isAuthenticated();
        }

        @Override
        public void setAuthenticated(boolean isAuthenticated)
                throws IllegalArgumentException {
            getSourceAuthentication().setAuthenticated(isAuthenticated);
        }
    }

    private String rewriteByStoreAndRepo(String path, String storageId, String repositoryId) {
        String[] split = path.split("/");
        if (split.length <= 4) {
            return path;
        } else {
            split[2] = storageId;
            split[3] = repositoryId;
            return String.join("/", split);
        }
    }

    private String parseRequestUri(String requestUri) {
        try {
            requestUri = UriUtils.decode(requestUri);
        } catch (Exception ex) {
            logger.error("Get requestUri error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return requestUri;
    }

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }

    public boolean handlerRestrictedRepository(List<GrantedAuthority> grantedAuthorities, String storageId, String repositoryId) {
        HttpServletRequest request = UrlUtils.getRequest();
        if (Objects.isNull(request)) {
            return false;
        }
        String serverName = request.getServerName();
        List<String> restrictedSourceList = getRestrictedSource();
        if (CollectionUtils.isEmpty(restrictedSourceList)) {
            return false;
        }
        if (!restrictedSourceList.contains(serverName)) {
            return false;
        }
        List<String> restrictedRepositoryList = getRestrictedRepository();
        if (CollectionUtils.isEmpty(restrictedRepositoryList)) {
            return false;
        }
        if (!restrictedRepositoryList.contains(ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId))) {
            grantedAuthorities.removeAll(Privileges.restricted());
            return true;
        }
        return false;
    }

    public List<String> getRestrictedSource() {
        List<String> restrictedSourceList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_SOURCE";
        String restrictedSource = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedSource)) {
            restrictedSourceList = Arrays.asList(restrictedSource.split(","));
        }
        return restrictedSourceList;
    }

    public List<String> getRestrictedRepository() {
        List<String> restrictedRepositoryList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_REPOSITORY";
        String restrictedRepository = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedRepository)) {
            restrictedRepositoryList = Arrays.asList(restrictedRepository.split(","));
        }
        return restrictedRepositoryList;
    }
    
}
