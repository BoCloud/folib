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

import com.alibaba.fastjson.JSONObject;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.constant.GlobalConstants;
import com.folib.data.criteria.Paginator;
import com.folib.domain.Artifact;
import com.folib.enums.ProductTypeEnum;
import com.folib.providers.io.*;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Component
public class HostedRepositoryProvider extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(HostedRepositoryProvider.class);

    private static final String ALIAS = "hosted";

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath repositoryPath) throws IOException {
        try {
            return Files.newInputStream(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            logger.info("The path [{}] does not exist!\n*\t[{}]", repositoryPath, e.getMessage());

            return null;
        } catch (IOException ex) {
            logger.error("Failed to decorate InputStream for [{}]", repositoryPath, ex);

            throw ex;
        }
    }

    @Override
    public OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        List<Path> result = new LinkedList<Path>();

        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        long startTime = System.currentTimeMillis();
        List<Artifact> searchResult = artifactIdGroupRepository.findArtifactsGremlin(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit());
        logger.info("FindArtifacts storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, predicate.getArtifactId(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit(), searchResult.size(), System.currentTimeMillis() - startTime);
        for (Artifact artifactEntry : searchResult) {

            try {
                result.add(rootRepositoryPath.resolve(artifactEntry));
            } catch (Exception e) {
                logger.error("Failed to resolve Artifact [{}]",
                        artifactEntry.getArtifactCoordinates(), e);
                continue;
            }
        }
        return result;
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId, predicate.getArtifactId(),
                predicate.getUseArtifactName(), predicate.getCoordinateValues());
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        logger.debug(" -> Checking local cache for {} ...", repositoryPath);
        repositoryPath = resolveRealPath(repositoryPath);
        if (artifactNotExists(repositoryPath)) {
            logger.info("The artifact {} was not found in the local cache", repositoryPath);
            return null;
        }
        logger.debug("The artifact {} was found in the local cache", repositoryPath);
        return repositoryPath;
    }

    private boolean artifactNotExists(RepositoryPath repositoryPath) throws IOException {
        return !RepositoryFiles.artifactExists(repositoryPath);
    }

    private RepositoryPath resolveRealPath(RepositoryPath repositoryPath) {
        try {
            Repository repository = repositoryPath.getRepository();
            if (!ProductTypeEnum.Raw.getFoLibraryName().equals(repository.getLayout()) || Boolean.FALSE.equals(repository.getEnableCustomLayout()) || StringUtils.isBlank(repository.getCustomLayout())) {
                return repositoryPath;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (GlobalConstants.LATEST_ARTIFACT_KEY_LIST.stream().noneMatch(artifactPath::contains)) {
                return repositoryPath;
            }
            final Pattern pattern = Pattern.compile(repository.getCustomLayout());
            RepositoryPath repositoryParentPath = getParentPath(pattern, repositoryPath);
            if (Objects.isNull(repositoryParentPath) || !Files.exists(repositoryParentPath)) {
                return repositoryPath;
            }
            List<RepositoryPath> repositoryPathList = Lists.newArrayList();
            try (Stream<Path> pathStream = Files.walk(repositoryParentPath)) {
                pathStream.forEach(p -> {
                    try {
                        RepositoryPath matcherRepositoryPath = matcherPath(pattern, p);
                        if (Objects.nonNull(matcherRepositoryPath) && !repositoryPathList.contains(matcherRepositoryPath)) {
                            repositoryPathList.add(matcherRepositoryPath);
                            repositoryPathList.sort(Comparator.comparing(o -> new ComparableVersion(o.getFileName().toString())));
                        }
                    } catch (Exception ex) {
                        logger.error("Handler repositoryPath list error [{}]", ExceptionUtils.getStackTrace(ex));
                    }
                });
            }
            if (CollectionUtils.isNotEmpty(repositoryPathList)) {
                if (artifactPath.contains(GlobalConstants.RELEASE_ARTIFACT_KEY)) {
                    RepositoryPath releaseRepositoryPath = releasePath(repositoryPathList);
                    if (Objects.nonNull(releaseRepositoryPath)) {
                        return releaseRepositoryPath;
                    }
                    return repositoryPath;
                } else {
                    return repositoryPathList.get(repositoryPathList.size() - 1);
                }
            }
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        return repositoryPath;
    }

    private RepositoryPath getParentPath(Pattern pattern, RepositoryPath repositoryPath) {
        try {
            if (RepositoryFiles.isHidden(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return null;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Matcher matcher = pattern.matcher(artifactPath);
            if (matcher.matches()) {
                String orgPath = matcher.group("orgPath");
                String module = matcher.group("module");
                RootRepositoryPath rootRepositoryPath = repositoryPath.getFileSystem().getRootDirectory();
                return rootRepositoryPath.resolve(orgPath).resolve(module);
            }
            return null;
        } catch (Exception ex) {
            logger.error("Path [{}] getParentPath error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private RepositoryPath matcherPath(Pattern pattern, Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            if (RepositoryFiles.isHidden(repositoryPath) || !RepositoryFiles.isArtifact(repositoryPath)) {
                return null;
            }
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Matcher matcher = pattern.matcher(artifactPath);
            if (matcher.matches()) {
                return repositoryPath;
            }
            return null;
        } catch (Exception ex) {
            logger.error("Path [{}] isMatcher error [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private RepositoryPath releasePath(List<RepositoryPath> repositoryPathList) throws IOException {
        List<RepositoryPath> releaseList = Lists.newArrayList();
        for (RepositoryPath paths : repositoryPathList) {
            String metaDataPath = paths.getTarget().toString();
            String name = paths.getFileName().toString();
            metaDataPath = metaDataPath.replace(name, String.format(".%s.foLibrary-metadata/metadata.json", name));
            if (Files.exists(Path.of(metaDataPath))) {
                JSONObject data = JSONObject.parseObject(Files.readString(Path.of(metaDataPath)));
                if (data != null && data.containsKey("RELEASE")) {
                    releaseList.add(paths);
                    releaseList.sort(Comparator.comparing(o -> new ComparableVersion(o.getFileName().toString())));
                }
            }
        }
       if(!releaseList.isEmpty()){
           return releaseList.get(releaseList.size() - 1);
       }
       return null;
    }

}
