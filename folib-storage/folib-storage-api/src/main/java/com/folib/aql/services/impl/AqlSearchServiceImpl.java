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
