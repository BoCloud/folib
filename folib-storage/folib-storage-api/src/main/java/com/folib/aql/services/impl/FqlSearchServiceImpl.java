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
package com.folib.aql.services.impl;

import com.folib.data.criteria.Selector;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.aql.services.AqlSearchService;
import com.folib.storage.search.SearchResults;
import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;

@Component
@Transactional
public class FqlSearchServiceImpl extends GremlinVertexRepository<Artifact> implements AqlSearchService {
    @Inject
    ArtifactAdapter artifactAdapter;

    @Inject
    ArtifactRepository artifactRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Inject
    private SnippetGenerator snippetGenerator;

    @Override
    public SearchResults search(Selector<ArtifactEntity> selector) throws IOException {

        return null;
    }

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }

}
