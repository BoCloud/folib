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

import com.folib.constant.DebianConstant;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.indexer.DebianIncrementalIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author veadan
 */
@Component
public class DebianArtifactEventListener {

    @Inject
    private DebianIncrementalIndexer debianIncrementalIndexer;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        final Repository repository = getRepository(event);

        if (!DebianLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }
        //  删除事件更新索引
        if (event.getType() == ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() || event.getType() == ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType()) {
            if (event.getPath().getPath().startsWith(DebianConstant.DEB_PREFIX)) {
                debianIncrementalIndexer.removeByPath(repository, event.getPath().getPath());
            }
        }
    }

    Repository getRepository(final ArtifactEvent<RepositoryPath> event) {
        return event.getPath().getFileSystem().getRepository();
    }
}
