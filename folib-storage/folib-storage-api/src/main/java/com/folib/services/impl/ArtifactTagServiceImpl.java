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

import java.lang.reflect.UndeclaredThrowableException;
import java.util.Optional;

import jakarta.inject.Inject;

import org.apache.tinkerpop.gremlin.structure.Graph;
import com.folib.artifact.ArtifactTag;
import com.folib.domain.ArtifactTagEntity;
import com.folib.gremlin.dsl.EntityTraversalSource;
import com.folib.repositories.ArtifactTagRepository;
import com.folib.services.ArtifactTagService;
import org.janusgraph.core.JanusGraph;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class ArtifactTagServiceImpl implements ArtifactTagService
{

    @Inject
    private ArtifactTagRepository artifactTagRepository;
    @Inject
    private JanusGraph janusGraph;

    @Override
    @Cacheable(value = "tags", key = "#name != null ? #name : 'default'")
    public ArtifactTag findOneOrCreate(String name)
    {
        Optional<ArtifactTag> optionalResult = artifactTagRepository.findById(name);

        return optionalResult.orElseGet(() -> {
            ArtifactTagEntity artifactTagEntry = new ArtifactTagEntity();
            artifactTagEntry.setName(name);

            Graph g = janusGraph.tx().createThreadedTx();
            try
            {
                ArtifactTagEntity result = artifactTagRepository.save(() -> g.traversal(EntityTraversalSource.class), artifactTagEntry);
                g.tx().commit();

                return result;
            }
            catch (Exception e)
            {
                g.tx().rollback();
                throw new UndeclaredThrowableException(e);
            }
            finally
            {
                g.tx().close();
            }
        });
    }

}
