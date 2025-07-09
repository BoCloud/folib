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
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;

import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenArtifactMetadataStoredEventListener
        extends BaseMavenArtifactEventListener
{

    @Inject
    private Maven2LayoutProvider maven2LayoutProvider;

    /**
     * Why not @{@link AsyncEventListener}:
     * <p>
     * Consider call to group repository for expired maven-metadata.xml.
     * Then all underlying sub-repositories will fetch their maven-metadata.xml.
     * In case at least one sub-repository was a proxy repository and its maven-metadata.xml local copy expired, we will re-fetch the maven-metadata.xml from remote.
     * Then this listener will be invoked and it will update all groups containing this proxy repository.
     * We need to have updated initial group when we return from the initial call.
     * Async update could be done a bit later.
     */
    @EventListener
    public void handle(final ArtifactEvent<RepositoryPath> event)
    {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout()))
        {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_STORED.getType())
        {
            return;
        }

        if (!maven2LayoutProvider.requiresGroupAggregation(event.getPath()))
        {
            return;
        }

        updateMetadataInGroupsContainingRepository(event, RepositoryPath::getParent);
    }

}
