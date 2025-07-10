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
package com.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.ConanArtifactIndex;
import com.folib.components.DistributedLockComponent;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ConanPackagesRevisions;
import com.folib.domain.ConanRevision;
import com.folib.domain.ConanRevisions;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.ConanLayoutProvider;
import com.folib.services.ArtifactIndexService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2024/3/20
 **/
@Slf4j
@Service
public class ArtifactIndexServiceImpl implements ArtifactIndexService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Override
    public void rebuildIndex(String storageId, String repositoryId, String artifactPath) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (!ConanLayoutProvider.ALIAS.equals(repository.getLayout())) {
            log.warn("Trying to rebuild index of repository {} with unsupported layout {} ", repository.getId(),
                    repository.getLayout());
            return;
        }

        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(repository);
        if (artifactPath != null && artifactPath.trim().length() > 0) {
            repositoryBasePath = repositoryBasePath.resolve(artifactPath);
        }
        if (!Files.exists(repositoryBasePath)) {
            return;
        }
        String key = "ConanIndex_" + repositoryBasePath.toString();
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                try (Stream<Path> pathStream = Files.walk(repositoryBasePath)) {
                    pathStream.filter(Files::isDirectory)
                            // Skip directories which start with a dot (like, for example: .index)
                            .filter(ConanArtifactIndex::isIndexDirectory)
                            // Note: Sorting can be expensive:
                            .sorted()
                            .forEach(this::execute);
                } catch (IOException ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    @Override
    public void rebuildIndex(RepositoryPath repositoryPath) {
        try {
            rebuildIndex(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    private void execute(Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            ConanArtifactIndex conanArtifactIndex = ConanArtifactIndex.parse(RepositoryFiles.relativizePath(repositoryPath));
            if (Objects.isNull(conanArtifactIndex)) {
                return;
            }
            String user = conanArtifactIndex.getUser(), name = conanArtifactIndex.getName(), version = conanArtifactIndex.getVersion(), channel = conanArtifactIndex.getChannel(), revisionId = conanArtifactIndex.getRevisionId(), packageId = conanArtifactIndex.getPackageId();
            Map<String, Long> map = Maps.newHashMap();
            try (Stream<Path> pathStream = Files.list(repositoryPath)) {
                pathStream.filter(ConanArtifactIndex::include).forEach(item -> {
                    try {
                        BasicFileAttributes attributes = Files.readAttributes(item, BasicFileAttributes.class);
                        map.put(item.getFileName().toString(), attributes.lastModifiedTime().toMillis());
                    } catch (Exception ex) {
                        log.error(ExceptionUtils.getStackTrace(ex));
                    }
                });
            }
            RepositoryPath indexJsonRepositoryPath = repositoryPath.resolve(ConanArtifactIndex.INDEX_JSON_NAME);
            if (MapUtils.isEmpty(map)) {
                Files.deleteIfExists(indexJsonRepositoryPath);
                return;
            }
            List<ConanRevision> revisions = Lists.newArrayList();
            map.entrySet().stream()
                    .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                    .forEach(entry -> {
                        revisions.add(ConanRevision.builder().revision(entry.getKey()).time(CommonUtils.getConvertMillis2String(entry.getValue())).build());
                    });
            if (StringUtils.isBlank(revisionId)) {
                String reference = String.format("%s/%s@%s/%s", name, version, user, channel);
                ConanRevisions conanRevisions = ConanRevisions.builder().reference(reference).build();
                conanRevisions.setRevisions(revisions);
                Files.writeString(indexJsonRepositoryPath, JSONObject.toJSONString(conanRevisions));
            } else {
                String packageReference = String.format("%s/%s@%s/%s#%s:%s", name, version, user, channel, revisionId, packageId);
                ConanPackagesRevisions conanPackagesRevisions = ConanPackagesRevisions.builder().packageReference(packageReference).build();
                conanPackagesRevisions.setRevisions(revisions);
                Files.writeString(indexJsonRepositoryPath, JSONObject.toJSONString(conanPackagesRevisions));
            }
            if (StringUtils.isNotBlank(conanArtifactIndex.getRootIndexRelativizePath())) {
                execute(repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), conanArtifactIndex.getRootIndexRelativizePath()));
            }
        } catch (Exception ex) {
            log.error("Rebuild index error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }
}
