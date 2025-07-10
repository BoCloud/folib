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

import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.constants.PypiConstants;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.PypiRepositoryFeatures;
import com.folib.repository.PypiRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;

/**
 * @author Veadan
 */
@Component
public class PypiLayoutProvider
        extends AbstractLayoutProvider<PypiCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(PypiLayoutProvider.class);

    public static final String ALIAS = PypiCoordinates.LAYOUT_NAME;

    @Inject
    private PypiRepositoryStrategy pypiRepositoryManagementStrategy;

    @Inject
    private PypiRepositoryFeatures pypiRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PypiCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        return PypiCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        // TODO: Fix
        return isPypiPackageHtml(path);
    }

    public boolean isMetadata(RepositoryPath path) {
        // TODO: Fix
        return isPypiPackageHtml(path);
    }

    public boolean isPypiPackageHtml(RepositoryPath path) {
        return path.getFileName().toString().endsWith(".html");
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
                    value = (Boolean) value && !isMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isPypiPackageHtml(repositoryPath)
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
        return pypiRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return pypiRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public void targetUrl(RepositoryPath repositoryPath) {
        String remoteUrl = repositoryPath.getRepository().getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        if (remoteUrl.endsWith(PypiConstants.PYPI_SIMPLE)) {
            try {
                String artifactPath = RepositoryFiles.relativizePath(repositoryPath), imagePath = "";
                PypiCoordinates pypiArtifactCoordinates = PypiCoordinates.parse(artifactPath);
                String path = pypiArtifactCoordinates.getPath();
                remoteUrl = remoteUrl.replace(PypiConstants.PYPI_SIMPLE, PypiConstants.PYPI_PACKAGES);
                repositoryPath.setTargetUrl(path);
            } catch (Exception ex) {
                logger.warn("RepositoryPath [{}] parse coordinates error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
        if (StringUtils.isBlank(repositoryPath.getTargetUrl())) {
            return;
        }
        repositoryPath.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(repositoryPath.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

}
