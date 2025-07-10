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
package com.folib.providers;

import cn.hutool.crypto.digest.SM3;
import com.alibaba.fastjson.JSONObject;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.artifact.coordinates.PubCoordinates;
import com.folib.constants.PubConstants;
import com.folib.domain.PubPackageMetadata;
import com.folib.domain.PubPackageVersionMetadata;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.PubRepositoryFeatures;
import com.folib.repository.PubRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Component
public class PubLayoutProvider
        extends AbstractLayoutProvider<PubCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(PubLayoutProvider.class);

    public static final String ALIAS = PubCoordinates.LAYOUT_NAME;

    @Lazy
    @Inject
    private PubRepositoryStrategy pubRepositoryManagementStrategy;
    @Lazy
    @Inject
    private PubRepositoryFeatures pubRepositoryFeatures;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PubCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException {
        return PubCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return isPubSpec(path);
    }

    public boolean isPubMetadata(RepositoryPath path) {
        return path.getFileName().toString().endsWith("pubspec.lock");
    }

    public boolean isPubSpec(RepositoryPath path) {
        return PubConstants.PUB_SPEC_YAML.equals(path.getFileName().toString());
    }

    public boolean isPubPackageJson(RepositoryPath path) {
        return path.getFileName().toString().endsWith(".json");
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
                    value = (Boolean) value && !isPubMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isPubMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isPubPackageJson(repositoryPath)
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
    public void targetUrl(RepositoryPath path) throws IOException {
        PubCoordinates pubArtifactCoordinates = null;
        URI uri = null;
        try {
            String artifactPath = RepositoryFiles.relativizePath(path);
            pubArtifactCoordinates = PubCoordinates.parse(artifactPath);
            String packagePath = PubConstants.PACKAGE_JSON_PATH + pubArtifactCoordinates.getName() + PubConstants.PACKAGE_JSON_EXTENSION;
            RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(path.getRepository(), packagePath);
            uri = pubArtifactCoordinates.convertToResource(pubArtifactCoordinates);
            if (Objects.isNull(packageJsonRepositoryPath) || !Files.exists(packageJsonRepositoryPath)) {
                throw new ArtifactNotFoundException(uri, String.format("Path packageName [%s] packageJsonRepositoryPath not exists", pubArtifactCoordinates.getName()));
            }
            PubPackageMetadata pubPackageMetadata = JSONObject.parseObject(Files.readString(packageJsonRepositoryPath), PubPackageMetadata.class);
            PubCoordinates finalPubArtifactCoordinates = pubArtifactCoordinates;
            Optional<PubPackageVersionMetadata> optionalPubPackageVersionMetadata = pubPackageMetadata.getVersions().stream().filter(item -> item.getVersion().equals(finalPubArtifactCoordinates.getVersion())).findFirst();
            if (optionalPubPackageVersionMetadata.isEmpty()) {
                throw new ArtifactNotFoundException(uri, String.format("Path packageName [%s] version [%s] not exists", pubArtifactCoordinates.getName(), pubArtifactCoordinates.getVersion()));
            }
            PubPackageVersionMetadata pubPackageVersionMetadata = optionalPubPackageVersionMetadata.get();
            path.setTargetUrl(pubPackageVersionMetadata.getSourceArchiveUrl());
        } catch (Exception ex) {
            logger.warn("RepositoryPath [{}] parse coordinates error [{}]", path, ExceptionUtils.getStackTrace(ex));
            if (ex instanceof ArtifactNotFoundException) {
                throw new ArtifactNotFoundException(uri, String.format("Path packageName [%s] version [%s] not exists", pubArtifactCoordinates.getName(), pubArtifactCoordinates.getVersion()));
            } else {
                throw new RuntimeException(ex.getMessage());
            }
        }
    }

    @Override
    public RepositoryStrategy getRepositoryManagementStrategy() {
        return pubRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return pubRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512, SM3.ALGORITHM_NAME)
                .collect(Collectors.toSet());
    }
}
