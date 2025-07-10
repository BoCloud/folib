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
package com.folib.event;

import com.folib.artifact.coordinates.ConanArtifactIndex;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.ConanLayoutProvider;
import com.folib.services.ArtifactIndexService;
import com.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Objects;

/**
 * @author veadan
 */
@Component
public class ConanArtifactEventListener {

    @Inject
    private ArtifactIndexService artifactIndexService;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!ConanLayoutProvider.ALIAS.equals(repository.getLayout())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType()) {
            return;
        }
        try {
            RepositoryPath repositoryPath = event.getPath();
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            ConanArtifactIndex conanArtifactIndex = ConanArtifactIndex.parse(artifactPath);
            if (Objects.isNull(conanArtifactIndex) || StringUtils.isBlank(conanArtifactIndex.getIndexRelativizePath())) {
                return;
            }
            artifactIndexService.rebuildIndex(storageId, repositoryId, conanArtifactIndex.getIndexRelativizePath());
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
