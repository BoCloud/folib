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

import com.folib.enums.PubIndexTypeEnum;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.indexer.PubPackageMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.PubLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * @author veadan
 */
@Component
public class PubArtifactEventListener {

    @Inject
    private PubPackageMetadataIndexer pubPackageMetadataIndexer;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!PubLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType() && event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType()) {
            return;
        }
        try {
            switch (event.getType()) {
                case 10:
                case 22:
                case 23:
                    pubPackageMetadataIndexer.indexAsSystem(event.getPath(), PubIndexTypeEnum.REINDEX);
                    break;
                case 16:
                    pubPackageMetadataIndexer.indexAsSystem(event.getPath(), PubIndexTypeEnum.DELETE);
                    break;
                default:
                    ;
            }
        } catch (Exception e) {
            throw new UndeclaredThrowableException(e);
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
