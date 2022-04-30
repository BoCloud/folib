package com.veadan.folib.services.impl;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import com.veadan.folib.data.criteria.OQueryTemplate;
import com.veadan.folib.data.criteria.QueryTemplate;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.services.AqlSearchService;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;

@Component
@Transactional
public class AqlSearchServiceImpl implements AqlSearchService
{

    //@PersistenceContext
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
        for (ArtifactEntity artifactEntry : queryTemplate.select(selector))
        {
            SearchResult r = new SearchResult();
            result.getResults().add(r);

            r.setStorageId(artifactEntry.getStorageId());
            r.setRepositoryId(artifactEntry.getRepositoryId());
            r.setArtifactCoordinates(artifactEntry.getArtifactCoordinates());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactEntry.getStorageId(),
                                                                           artifactEntry.getRepositoryId(),
                                                                           artifactEntry.getArtifactPath());

            Repository repository = repositoryPath.getRepository();

            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());

            List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(),
                                                                             artifactEntry.getArtifactCoordinates());
            r.setSnippets(snippets);
        }

        return result;
    }

}
