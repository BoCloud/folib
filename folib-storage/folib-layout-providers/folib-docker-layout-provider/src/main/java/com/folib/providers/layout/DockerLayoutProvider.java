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
package com.folib.providers.layout;

import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.DockerAuthComponent;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.enums.DockerHeaderEnum;
import com.folib.providers.header.HeaderMappingRegistry;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repository.DockerRepositoryFeatures;
import com.folib.repository.DockerRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.utils.DockerUtils;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Veadan
 */
@Component
public class DockerLayoutProvider
        extends AbstractLayoutProvider<DockerCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(DockerLayoutProvider.class);

    public static final String ALIAS = DockerCoordinates.LAYOUT_NAME;

    public static final String IMAGES_MANIFEST = "manifest.json";

    public static final String USER_AGENT_PREFIX = ALIAS;

    public static final String BLOBS = "blobs";

    public static final String MANIFEST = "manifest";

    public static final String MANIFESTS = "manifests";

    public static final List<String> DOCKER_BLOBS_MANIFESTS_URL = Lists.newArrayList(MANIFESTS, BLOBS);

    public static final List<String> DOCKER_SUBLAYOUT = Lists.newArrayList(DockerCoordinates.LAYOUT_NAME, "ollama");

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    @Inject
    private DockerRepositoryStrategy dockerRepositoryManagementStrategy;

    @Inject
    private DockerRepositoryFeatures dockerRepositoryFeatures;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private DockerAuthComponent dockerAuthComponent;

    @PostConstruct
    public void register() {
        headerMappingRegistry.register(ALIAS, USER_AGENT_PREFIX);

        logger.info("Registered layout provider '" + getClass().getCanonicalName() + "' with alias '" + ALIAS + "'.");
    }

    @Override
    public DockerCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        logger.debug("DockerArtifactCoordinates parse path [{}]", path);
        return DockerCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        // TODO: Fix
        return path.getFileName().toString().endsWith(".sha256");
    }

    public boolean isMetadata(RepositoryPath path) {
        // TODO: Fix
        return Objects.equals(IMAGES_MANIFEST, path.getFileName().toString());
    }

    public boolean isSubsidiaryFile(RepositoryPath path) {
       try {
           if (path.getRoot().toAbsolutePath().toString().equals(path.toAbsolutePath().toString())) {
               return false;
           }
           String parentFilename = path.getParent().getFileName().toString();
           return Objects.equals(DockerUtils.SUBSIDIARY_PATH, parentFilename);
       } catch (Exception ex) {
           logger.error(ExceptionUtils.getStackTrace(ex));
       }
       return false;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isMetadata(repositoryPath) && !isSubsidiaryFile(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case EXPIRED:
                    final Instant tenSecondsAgo = Instant.now().minus(10, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || ((DockerCoordinates.isDockerTag(repositoryPath) || DockerCoordinates.isRealManifestPath(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    tenSecondsAgo));

                    result.put(attributeType, value);

                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && (DockerCoordinates.isDockerTag(repositoryPath) || DockerCoordinates.isRealManifestPath(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo));

                    result.put(attributeType, value);

                    break;
                default:

                    break;
            }
        }

        return result;
    }

    @Override
    public RepositoryStrategy getRepositoryManagementStrategy() {
        return dockerRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return dockerRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.SHA_256).collect(Collectors.toSet());
    }

    @Override
    public void initData(String storageId, String repositoryId) {
        try {
            for (String key : GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST) {
                ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, key);
                artifactIdGroupRepository.saveOrUpdate(artifactIdGroup);
            }
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public void targetUrl(RepositoryPath path) {
        String remoteUrl = path.getRepository().getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        if (remoteUrl.endsWith(GlobalConstants.DOCKER_V2)) {
            try {
                String artifactPath = RepositoryFiles.relativizePath(path), imagePath = "";
                boolean concat = false;
                DockerCoordinates dockerArtifactCoordinates;
                if (DockerLayoutProvider.DOCKER_BLOBS_MANIFESTS_URL.stream().noneMatch(artifactPath::contains)) {
                    dockerArtifactCoordinates = DockerCoordinates.parse(artifactPath);
                    imagePath = dockerArtifactCoordinates.getName();
                    if (imagePath.split(GlobalConstants.SEPARATOR).length <= 1) {
                        concat = true;
                    }
                } else {
                    if(Objects.isNull(path.getTargetUrl())){
                        return;
                    }
                    dockerArtifactCoordinates = DockerCoordinates.parse(path.getTargetUrl());
                    imagePath = dockerArtifactCoordinates.getName();
                    if (imagePath.split(GlobalConstants.SEPARATOR).length <= 2) {
                        concat = true;
                    }
                }
                if (concat) {
                    remoteUrl = remoteUrl.concat(GlobalConstants.SEPARATOR).concat(GlobalConstants.DOCKER_DEFAULT_REPO);
                }
            } catch (Exception ex) {
                logger.warn("RepositoryPath [{}] parse coordinates error [{}]", path, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
        if (StringUtils.isBlank(path.getTargetUrl())) {
            return;
        }
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
        authToken(path);
        logger.debug("path [{}] docker headers [{}]", path, path.getHeaders());
    }

    private void authToken(RepositoryPath path) {
        try {
            RemoteRepository remoteRepository = path.getRepository().getRemoteRepository();
            String storageId = path.getStorageId(), repositoryId = path.getRepositoryId();
            DockerCoordinates dockerArtifactCoordinates = DockerCoordinates.parse(RepositoryFiles.relativizePath(path));
            path.setHeaders(DockerHeaderEnum.acceptHeaders());
            String imagePath = dockerArtifactCoordinates.getName();
            if (StringUtils.isNotBlank(path.getArtifactPath())) {
                imagePath = path.getArtifactPath();
            }
            dockerAuthComponent.handleAuthToken(remoteRepository, storageId, repositoryId, imagePath, path.getHeaders());
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
        }
    }
}
