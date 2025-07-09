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

import java.io.IOException;
import java.net.URL;
import java.util.List;

import jakarta.inject.Inject;


import com.folib.data.criteria.OQueryTemplate;
import com.folib.data.criteria.QueryTemplate;
import com.folib.data.criteria.Selector;
import com.folib.dependency.snippet.CodeSnippet;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.search.SearchResult;
import com.folib.storage.search.SearchResults;
import com.folib.domain.ArtifactEntity;
import com.folib.aql.services.AqlSearchService;
import com.folib.storage.repository.Repository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;

@Component
@Transactional
public class AqlSearchServiceImpl implements AqlSearchService
{

//    @PersistenceContext
    private EntityManager entityManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private SnippetGenerator snippetGenerator;

    public SearchResults search(Selector<ArtifactEntity> selector)
        throws IOException
    {
        SearchResults result = new SearchResults();

        QueryTemplate<List<ArtifactEntity>, ArtifactEntity> queryTemplate = new OQueryTemplate<>(entityManager);
        for (ArtifactEntity artifactEntity : queryTemplate.select(selector))
        {
            SearchResult r = new SearchResult();
            result.getResults().add(r);

            r.setStorageId(artifactEntity.getStorageId());
            r.setRepositoryId(artifactEntity.getRepositoryId());
            r.setArtifactCoordinates(artifactEntity.getArtifactCoordinates());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactEntity.getStorageId(),
                                                                           artifactEntity.getRepositoryId(),
                                                                           artifactEntity.getArtifactPath());

            Repository repository = repositoryPath.getRepository();

            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());

            List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(),
                                                                             artifactEntity.getArtifactCoordinates());
            r.setSnippets(snippets);
        }

        return result;
    }

}
