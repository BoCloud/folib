package com.folib.services.impl;

import com.folib.providers.search.SearchException;
import com.folib.providers.search.SearchProvider;
import com.folib.services.ArtifactSearchService;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResults;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactSearchServiceImpl
        implements ArtifactSearchService
{

    private SearchProvider searchProvider;


    @Override
    public SearchResults search(SearchRequest searchRequest)
            throws SearchException
    {
        return searchProvider.search(searchRequest);
    }

    @Override
    public boolean contains(SearchRequest searchRequest)
            throws SearchException
    {
        return !search(searchRequest).getResults().isEmpty();
    }

}
