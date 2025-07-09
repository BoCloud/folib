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
package com.folib.providers.search;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import jakarta.inject.Inject;

import com.folib.configuration.ConfigurationManager;
import com.folib.dependency.snippet.CodeSnippet;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.configuration.Configuration;
import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Veadan
 */
public abstract class AbstractSearchProvider
        implements SearchProvider
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractSearchProvider.class);
    
    @Inject
    private ArtifactRepository artifactEntityRepository;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SnippetGenerator snippetGenerator;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;


    @Override
    public SearchResult findExact(SearchRequest searchRequest)
    {
        Artifact artifactEntry = artifactEntityRepository.findOneArtifact(searchRequest.getStorageId(),
                                                                          searchRequest.getRepositoryId(),
                                                                          searchRequest.getArtifactCoordinates()
                                                                                       .buildPath());

        if (artifactEntry == null)
        {
            return null;
        }
        
        SearchResult searchResult = createSearchResult(artifactEntry);

        Storage storage = getConfiguration().getStorage(artifactEntry.getStorageId());
        Repository repository = storage.getRepository(searchRequest.getRepositoryId());

        List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(),
                                                                         artifactEntry.getArtifactCoordinates());
        searchResult.setSnippets(snippets);

        return searchResult;
    }

    @Override
    public boolean contains(SearchRequest searchRequest)
            throws SearchException
    {
        return !search(searchRequest).getResults().isEmpty();
    }

    protected SearchResult createSearchResult(Artifact a)
    {
        String storageId = a.getStorageId();
        
        URL artifactResource;
        try
        {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(a.getStorageId(), a.getRepositoryId(), a.getArtifactPath());
            artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
        }
        catch (IOException e)
        {
            logger.error("Failed to resolve artifact resource for [{}]",
                         a.getArtifactCoordinates(), e);
            return null;
        }

        return new SearchResult(storageId,
                                a.getRepositoryId(),
                                a.getArtifactCoordinates(),
                                artifactResource.toString());
    }

    public Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

}
