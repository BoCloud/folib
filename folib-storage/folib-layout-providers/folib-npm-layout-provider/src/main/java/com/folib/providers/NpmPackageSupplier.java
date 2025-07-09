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

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.config.NpmLayoutProviderConfig;
import com.folib.domain.Artifact;
import com.folib.enums.NpmSubLayout;
import com.folib.npm.metadata.Dependency;
import com.folib.npm.metadata.Dist;
import com.folib.npm.metadata.PackageVersion;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactTagService;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.time.ZoneId;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * @author veadan
 */
@Component
public class NpmPackageSupplier implements Function<Path, NpmPackageDesc> {

    private static final Logger logger = LoggerFactory.getLogger(NpmPackageSupplier.class);

    @Inject
    private NpmLayoutProvider layoutProvider;

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    @NpmLayoutProviderConfig.NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Override
    public NpmPackageDesc apply(Path path) {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        NpmFileSystemProvider npmFileSystemProvider = (NpmFileSystemProvider) path.getFileSystem().provider();

        NpmCoordinates c;
        Artifact artifactEntry;
        try {
            c = (NpmCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }

        NpmPackageDesc npmPackageDesc = new NpmPackageDesc();
        Date releaseDate = Date.from(artifactEntry.getCreated().atZone(ZoneId.systemDefault()).toInstant());
        if (Objects.nonNull(artifactEntry.getLastUpdated())) {
            releaseDate = Date.from(artifactEntry.getLastUpdated().atZone(ZoneId.systemDefault()).toInstant());
        }
        npmPackageDesc.setReleaseDate(releaseDate);

        PackageVersion npmPackage = null;
        String packagePath = NpmSubLayout.OHPM.getValue().equals(repositoryPath.getRepository().getSubLayout()) ? NpmLayoutProvider.OHPM_PACKAGE_JSON_PATH : NpmLayoutProvider.DEFAULT_PACKAGE_JSON_PATH;
        byte[] packageJsonBytes = layoutProvider.getContentByFileName(repositoryPath, repositoryPath, packagePath);

        if (Objects.nonNull(packageJsonBytes)) {
            String packageJson = new String(packageJsonBytes, StandardCharsets.UTF_8);
            try {
                npmPackage = npmJacksonMapper.readValue(packageJson, PackageVersion.class);
            } catch (Exception ex) {
                logger.warn("Artifact packageVersion 转换异常 [{}]", artifactEntry.getUuid());
            }
        }
        if (Objects.isNull(npmPackage)) {
            npmPackage = new PackageVersion();
        }
        npmPackageDesc.setNpmPackage(npmPackage);

        npmPackage.setAdditionalProperty("_id", String.format("%s@%s", c.getId(), c.getVersion()));

        npmPackage.setName(c.getId());
        npmPackage.setVersion(c.getVersion());
        Dist dist = new Dist();
        npmPackage.setDist(dist);

        if (StringUtils.isNotBlank(artifactEntry.getDependencies())) {
            JSONObject dependenciesJson = JSONObject.parseObject(artifactEntry.getDependencies());
            Dependency dependency = new Dependency();
            String value = "";
            for (Map.Entry<String, Object> entry : dependenciesJson.entrySet()) {
                if (Objects.nonNull(entry.getValue())) {
                    value = entry.getValue().toString();
                }
                dependency.setAdditionalProperty(entry.getKey(), value);
            }
            npmPackage.setDependencies(dependency);
        }

        Map<String, RepositoryPath> checksumMap = npmFileSystemProvider.resolveChecksumPathMap(repositoryPath);
        fetchShasum(dist, checksumMap);

        String url;
        try {
            url = layoutProvider.resolveResource(repositoryPath).toString();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
        dist.setTarball(url);

        return npmPackageDesc;
    }

    private void fetchShasum(Dist dist,
                             Map<String, RepositoryPath> checksumMap) {
        RepositoryPath shasumPath = checksumMap.get(MessageDigestAlgorithms.SHA_1);
        RepositoryPath integrityPath = checksumMap.get(MessageDigestAlgorithms.SHA_512);
        try {
            if (Objects.nonNull(shasumPath) && Files.exists(shasumPath)) {
                dist.setShasum(new String(Files.readAllBytes(shasumPath), StandardCharsets.UTF_8).trim());
            }
            if (Objects.nonNull(integrityPath) && Files.exists(integrityPath)) {
                dist.setIntegrity(new String(Files.readAllBytes(integrityPath), StandardCharsets.UTF_8).trim());
            }
        } catch (NoSuchFileException e) {
            logger.info("Checksum file not found [{}].", shasumPath);
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
    }

}
