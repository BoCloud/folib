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

import com.google.common.collect.Iterables;
import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.domain.ArtifactTagEntity;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.repository.Repository;
import com.folib.util.CommonUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

import jakarta.inject.Inject;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @author veadan
 */
@Service
@Transactional
public class ArtifactIdGroupServiceImpl
        extends AbstractArtifactGroupService<ArtifactIdGroup>
        implements ArtifactIdGroupService {

    private static final Logger logger = LoggerFactory.getLogger(ArtifactIdGroupEntity.class);

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Override
    public void saveArtifacts(Repository repository,
                              Set<Artifact> artifactToSaveSet) {
        try {
            Map<String, List<Artifact>> artifactByGroupIdMap = artifactToSaveSet.stream()
                    .collect(Collectors.groupingBy(a -> a.getArtifactCoordinates()
                            .getId()));
            ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
            List<Artifact> artifacts = null;
            String artifactGroupId = "";
            for (Entry<String, List<Artifact>> artifactIdGroupEntry : artifactByGroupIdMap.entrySet()) {
                artifacts = artifactIdGroupEntry.getValue();
                artifactGroupId = artifactIdGroupEntry.getKey();
                for (List<Artifact> artifactsBatch : Iterables.partition(artifacts, 20)) {
                    mergeArtifactIdGroup(repository, artifactGroupId, lastVersionTag, artifactsBatch);
                }
            }
        } catch (Exception e) {
            logger.error("saveArtifacts error：{}", ExceptionUtils.getStackTrace(e));
            throw new RuntimeException(e);
        }
    }

    private void mergeArtifactIdGroup(Repository repository, String artifactGroupId, ArtifactTag lastVersionTag, List<Artifact> artifactsBatch) {
        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactsGroupWithTag(repository.getStorage()
                        .getId(),
                repository.getId(),
                artifactGroupId,
                Optional.of(lastVersionTag))
                .orElseGet(() -> create(repository.getStorage().getId(),
                        repository.getId(),
                        artifactGroupId));

        ArtifactCoordinates lastVersion = addArtifactsToGroup(artifactsBatch, artifactGroup);
        logger.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());

        try {
            artifactIdGroupRepository.saveOrUpdate(artifactGroup);
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            if (CommonUtils.catchException(realMessage)) {
                logger.warn("Merge group [{}] is [{}] with [{}] error [{}]",
                        artifactGroup.getName(),
                        lastVersion.getVersion(),
                        lastVersion.getPath(),
                        realMessage);
            } else {
                throw new RuntimeException(ex.getMessage());
            }
        }
    }

    private ArtifactCoordinates addArtifactsToGroup(List<Artifact> artifacts,
                                                    ArtifactIdGroup artifactGroup) {
        ArtifactCoordinates lastVersion = null;
        for (Artifact artifact : artifacts) {
            lastVersion = addArtifactToGroup(artifactGroup, artifact);
        }
        return lastVersion;
    }

    @Override
    public ArtifactCoordinates addArtifactToGroup(ArtifactIdGroup artifactGroup,
                                                  Artifact artifact) {
        ArtifactCoordinates coordinates = artifact.getArtifactCoordinates();
        Assert.notNull(coordinates, "coordinates should not be null");

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        artifact.getTagSet().add(lastVersionTag);
        artifactGroup.addArtifact(artifact);

        Artifact lastVersionArtifact = artifactGroup.getArtifacts()
                .stream()
                .filter(e -> e.getTagSet().contains(lastVersionTag))
                .reduce((a1,
                         a2) -> reduceByLastVersionTag(a1, a2, lastVersionTag))
                .get();

        return lastVersionArtifact.getArtifactCoordinates();
    }

    private Artifact reduceByLastVersionTag(Artifact a1,
                                            Artifact a2,
                                            ArtifactTag lastVersionTag) {
        int artifactCoordinatesComparison = a1.getArtifactCoordinates()
                .compareTo(a2.getArtifactCoordinates());
        if (artifactCoordinatesComparison > 0) {
            a2.getTagSet().remove(lastVersionTag);
            return a1;
        } else if (artifactCoordinatesComparison < 0) {
            a1.getTagSet().remove(lastVersionTag);
            return a2;
        }

        return a1;
    }

    protected ArtifactIdGroup create(String storageId,
                                     String repositoryId,
                                     String artifactId) {
        return new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
    }

}
