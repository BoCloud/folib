package com.veadan.folib.providers.search;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.search.SearchRequest;
import com.veadan.folib.storage.search.SearchResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author carlspring
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
