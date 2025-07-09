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
package com.folib.providers.repository;


import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.components.ArtifactSecurityComponent;
import com.folib.configuration.ConfigurationUtils;
import com.folib.data.criteria.Paginator;
import com.folib.enums.ProductTypeEnum;
import com.folib.interceptor.GroupInterceptor;
import com.folib.providers.io.AbstractRepositoryProvider;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.repository.event.GroupRepositoryPathFetchEvent;
import com.folib.providers.repository.group.GroupRepositorySetCollector;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.support.ArtifactRoutingRulesChecker;
import com.folib.storage.Storage;
import com.folib.storage.metadata.MetadataHelper;
import com.folib.storage.repository.Repository;
import com.folib.users.domain.Privileges;
import com.folib.util.ThrowingFunction;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.index.artifact.M2ArtifactRecognizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Component
public class GroupRepositoryProvider
        extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(GroupRepositoryProvider.class);

    private static final String ALIAS = "group";

    @Inject
    private ArtifactRoutingRulesChecker artifactRoutingRulesChecker;

    @Inject
    private HostedRepositoryProvider hostedRepositoryProvider;

    @Inject
    private GroupRepositorySetCollector groupRepositorySetCollector;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactResolutionService artifactResolutionService;

    @Autowired
    @Lazy
    private ArtifactSecurityComponent artifactSecurityComponent;

    @Resource
    private GroupInterceptor groupInterceptor;


    private static final String CONFIG_JSON = "config.json";
    private static final String XML_EXTENSION = ".xml";
    private static final String XML_GZ_EXTENSION = ".xml.gz";

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath path)
            throws IOException {
        return hostedRepositoryProvider.getInputStreamInternal(path);
    }

    @Override
    public RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        if (Objects.isNull(repositoryPath)) {
            return null;
        }
        boolean mavenMetadata = ProductTypeEnum.Maven.getFoLibraryName().equals(repositoryPath.getRepository().getLayout()) &&
                MetadataHelper.MAVEN_METADATA_XML.equals(repositoryPath.getFileName().toString()) &&
                !M2ArtifactRecognizer.isSnapshot(repositoryPath.getParent().getFileName().toString());
        if (mavenMetadata) {
            eventPublisher.publishEvent(new GroupRepositoryPathFetchEvent(repositoryPath));
        }
        if (repositoryPath.toString().endsWith(MetadataHelper.MAVEN_METADATA_XML)) {
            RepositoryPath result = resolvePathDirectlyFromGroupPathIfPossible(repositoryPath);
            if (result != null) {
                return result;
            }
        }
        if(ProductTypeEnum.Cargo.getFoLibraryName().equals(repositoryPath.getRepository().getLayout()) && repositoryPath.toString().endsWith("config.json")){
            RepositoryPath result = resolvePathDirectlyFromGroupPathIfPossible(repositoryPath);
            if (result != null) {
                return result;
            }
        }
        if(ProductTypeEnum.Rpm.getFoLibraryName().equals(repositoryPath.getRepository().getLayout()) && (repositoryPath.toString().endsWith(".xml") || repositoryPath.toString().endsWith(".xml.gz"))){
            RepositoryPath result = resolvePathDirectlyFromGroupPathIfPossible(repositoryPath);
            if (result != null) {
                return result;
            }
        }
        if(groupInterceptor.shouldInterceptor(repositoryPath)){
            groupInterceptor.calculateIndex(repositoryPath);
            RepositoryPath result = resolvePathDirectlyFromGroupPathIfPossible(repositoryPath);
            return result;
        }


        return resolvePathTraversal(repositoryPath);
    }

    protected RepositoryPath resolvePathTraversal(RepositoryPath repositoryPath)
            throws IOException {
        Repository groupRepository = repositoryPath.getRepository();
        Storage storage = groupRepository.getStorage();

        // Iterate over the `repositories` collection.
        RepositoryPath subRepositoryPath = null;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(groupRepository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                //先从各个仓库本地缓存中查找一次，若存在则使用
                String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);

                Repository subRepository = getConfiguration().getStorage(sId).getRepository(rId);

                subRepositoryPath = repositoryPathResolver.resolve(subRepository, repositoryPath);
                if (!isRepositoryResolvable(groupRepository, subRepository, subRepositoryPath)) {
                    continue;
                }
                if (!artifactSecurityComponent.validatePrivileges(subRepositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                    continue;
                }
                if (Objects.nonNull(repositoryPath.getDisableRemote())) {
                    subRepositoryPath.setDisableRemote(repositoryPath.getDisableRemote());
                }
                if (StringUtils.isNotBlank(repositoryPath.getTargetUrl())) {
                    subRepositoryPath.setTargetUrl(repositoryPath.getTargetUrl());
                }
                if (MapUtils.isNotEmpty(repositoryPath.getHeaders())) {
                    subRepositoryPath.setHeaders(repositoryPath.getHeaders());
                }
                if (StringUtils.isNotBlank(repositoryPath.getArtifactPath())) {
                    subRepositoryPath.setArtifactPath(repositoryPath.getArtifactPath());
                }
                if (Objects.nonNull(subRepositoryPath) && Objects.nonNull(resolvePathDirectlyFromGroupPathIfPossible(subRepositoryPath))) {
                    logger.info("Located artifact: [{}]", subRepositoryPath);
                    return subRepositoryPath;
                }
            } catch (Exception ex) {
                logger.error("group repository resolvePathTraversal artifact: [{}] error：[{}]", subRepositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }

        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);

                Repository subRepository = getConfiguration().getStorage(sId).getRepository(rId);

                subRepositoryPath = repositoryPathResolver.resolve(subRepository, repositoryPath);
                if (!isRepositoryResolvable(groupRepository, subRepository, subRepositoryPath)) {
                    continue;
                }
                if (!artifactSecurityComponent.validatePrivileges(subRepositoryPath, Privileges.ARTIFACTS_RESOLVE.getAuthority())) {
                    continue;
                }
                if (Objects.nonNull(repositoryPath.getDisableRemote())) {
                    subRepositoryPath.setDisableRemote(repositoryPath.getDisableRemote());
                }
                if (StringUtils.isNotBlank(repositoryPath.getTargetUrl())) {
                    subRepositoryPath.setTargetUrl(repositoryPath.getTargetUrl());
                }
                if (MapUtils.isNotEmpty(repositoryPath.getHeaders())) {
                    subRepositoryPath.setHeaders(repositoryPath.getHeaders());
                }
                if (StringUtils.isNotBlank(repositoryPath.getArtifactPath())) {
                    subRepositoryPath.setArtifactPath(repositoryPath.getArtifactPath());
                }
                subRepositoryPath = resolvePathFromGroupMemberOrTraverse(subRepositoryPath);
                if (subRepositoryPath == null) {
                    continue;
                }
                logger.info("Located artifact: [{}]", subRepositoryPath);
            } catch (Exception ex) {
                logger.error("group repository resolvePathTraversal artifact: [{}] error：[{}]", subRepositoryPath, ExceptionUtils.getStackTrace(ex));
            }
            if (Objects.nonNull(subRepositoryPath) && Objects.nonNull(resolvePathDirectlyFromGroupPathIfPossible(subRepositoryPath))) {
                return subRepositoryPath;
            }
        }

        return null;
    }

    private boolean isRepositoryResolvable(Repository groupRepository,
                                           Repository subRepository,
                                           RepositoryPath repositoryPath)
            throws IOException {
        final boolean isInService = subRepository.isInService();

        if (!isInService) {
            logger.info("- Repository [{}] is not in service, skipping...",
                    subRepository.getStorageIdAndRepositoryId());

            // early break to avoid wasting time on looping through the routing rules.
            return false;
        }

        final boolean isRoutable = !artifactRoutingRulesChecker.isDenied(groupRepository, repositoryPath);

        if (!isRoutable) {
            logger.info("- Repository [{}] is denied by a routing rule, skipping...",
                    subRepository.getStorageIdAndRepositoryId());
        }

        return isInService && isRoutable;
    }

    private RepositoryPath resolvePathDirectlyFromGroupPathIfPossible(final RepositoryPath artifactPath) {
        try {
            return hostedRepositoryProvider.fetchPath(artifactPath);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    protected RepositoryPath resolvePathFromGroupMemberOrTraverse(RepositoryPath repositoryPath)
            throws IOException {
        Repository repository = repositoryPath.getRepository();
        if (getAlias().equals(repository.getType())) {
            return resolvePathTraversal(repositoryPath);
        }

        RepositoryProvider provider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) provider.fetchPath(repositoryPath);
        } catch (IOException e) {
            logger.error("Failed to resolve path [{}]", repositoryPath);
            return null;
        }
    }



    @Override
    protected OutputStream getOutputStreamInternal(RepositoryPath repositoryPath) throws IOException {
        // It should not be possible to write artifacts to a group repository.
        // A group repository should only serve artifacts that already exist
        // in the repositories within the group.

        String pathString = repositoryPath.toString();
        String layout = repositoryPath.getRepository().getLayout();

        if (isAllowedPath(layout, pathString)) {
            validatePath(repositoryPath);
            return Files.newOutputStream(repositoryPath);
        }
        logger.error("Invalid path: {}", pathString);
        throw new UnsupportedOperationException("Writing to this repository type is not supported");
    }

    private boolean isAllowedPath(String layout, String pathString) {
        if (ProductTypeEnum.Cargo.getFoLibraryName().equals(layout)) {
            return pathString.endsWith(CONFIG_JSON);
        } else if (ProductTypeEnum.Rpm.getFoLibraryName().equals(layout)) {
            return pathString.endsWith(XML_EXTENSION) || pathString.endsWith(XML_GZ_EXTENSION);
        }else if(ProductTypeEnum.Debian.getFoLibraryName().equals(layout)) {
            return true;
        }
        return false;
    }

    private void validatePath(RepositoryPath repositoryPath) {
        // Add path validation logic here to prevent path traversal attacks
        // For example, check if the path is within allowed directories
        // This is a placeholder for actual validation logic
        if (!isValidPath(repositoryPath)) {
            throw new SecurityException("Invalid path: " + repositoryPath);
        }
    }

    private boolean isValidPath(RepositoryPath repositoryPath) {
        // Implement actual path validation logic
        // Return true if the path is valid, false otherwise
        return true; // Placeholder implementation
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        logger.info("Search in [{}]:[{}] ...", storageId, repositoryId);

        Map<ArtifactCoordinates, Path> resultMap = new LinkedHashMap<>();

        Storage storage = getConfiguration().getStorage(storageId);
        Repository groupRepository = storage.getRepository(repositoryId);
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(groupRepository);

        if (groupRepositorySet.isEmpty()) {
            return new LinkedList<>();
        }

        long skip = paginator.getSkip();
        int limit = paginator.getLimit();

        int groupSize = groupRepositorySet.size();
        long groupSkip = (skip / (limit * groupSize)) * limit;
        int groupLimit = limit;

        skip = skip - groupSkip;

        outer:
        do {
            Paginator paginatorLocal = new Paginator();
            paginatorLocal.setLimit(groupLimit);
            paginatorLocal.setSkip(groupSkip);
            paginatorLocal.setProperty(paginator.getProperty());
            paginatorLocal.setOrder(paginator.getOrder());
            paginatorLocal.setUseLimit(paginator.getUseLimit());
            groupLimit = 0;

            for (Iterator<Repository> i = groupRepositorySet.iterator(); i.hasNext(); ) {
                Repository r = i.next();
                RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(r.getType());

                List<Path> repositoryResult = repositoryProvider.search(r.getStorage().getId(), r.getId(), predicate,
                        paginatorLocal);
                if (repositoryResult.isEmpty()) {
                    i.remove();
                    continue;
                }

                // count coordinates intersection
                groupLimit += repositoryResult.stream()
                        .map(ThrowingFunction.unchecked((Path p) -> resultMap.put(getArtifactCoordinates(p), p)))
                        .filter(p -> p != null)
                        .collect(Collectors.toList())
                        .size();

                //Break search iterations if we have reached enough list size.
                if (resultMap.size() >= limit + skip) {
                    break outer;
                }
            }
            groupSkip += limit;

            // Will iterate until there is no more coordinates intersection and
            // there is more search results within group repositories
        }
        while (groupLimit > 0 && !groupRepositorySet.isEmpty());

        LinkedList<Path> resultList = new LinkedList<>();
        if (skip >= resultMap.size()) {
            return resultList;
        }
        resultList.addAll(resultMap.values());

        long toIndex = resultList.size() - skip > limit ? limit + skip : resultList.size();
        return resultList.subList((int) skip, (int) toIndex);
    }

    private ArtifactCoordinates getArtifactCoordinates(Path p) throws IOException {
        return RepositoryFiles.readCoordinates((RepositoryPath) p);
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        logger.info("Count in [{}]:[{}] ...", storageId, repositoryId);

        Storage storage = getConfiguration().getStorage(storageId);
        Repository groupRepository = storage.getRepository(repositoryId);
        Set<Repository> groupRepositorySet = groupRepositorySetCollector.collect(groupRepository);
        Long count = 0L;
        if (groupRepositorySet.isEmpty()) {
            return count;
        }
        for (Iterator<Repository> i = groupRepositorySet.iterator(); i.hasNext(); ) {
            Repository r = i.next();
            RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(r.getType());
            count = count + repositoryProvider.count(r.getStorage().getId(), r.getId(), predicate);
        }
        return count;
    }
}
