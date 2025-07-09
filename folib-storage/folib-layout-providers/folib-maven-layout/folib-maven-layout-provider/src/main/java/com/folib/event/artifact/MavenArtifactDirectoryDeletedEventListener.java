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
package com.folib.event.artifact;

import com.folib.event.AsyncEventListener;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.File;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;

/**
 * @author veadan
 */
@Component
public class MavenArtifactDirectoryDeletedEventListener
        extends BaseMavenArtifactEventListener {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType()) {
            return;
        }

        try {
            String storageId = repository.getStorage().getId();
            String repositoryId = repository.getId();
            String path = event.getPath().toUri().getPath();
            String format = "%s/%s/";
            if (path.startsWith(File.separator)) {
                format = "/%s/%s/";
            }
            String artifactPath = path.replace(String.format(format, storageId, repositoryId), "");
            if (StringUtils.isNotBlank(artifactPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                if (Files.exists(repositoryPath.getParent()) && Files.isSameFile(repositoryPath.getRoot(), repositoryPath.getParent())) {
                    return;
                }
                artifactMetadataService.rebuildMetadata(storageId, repositoryId, artifactPath);
            }
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }
}
