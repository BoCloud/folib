package com.veadan.folib.services.impl;

import com.veadan.folib.providers.search.SearchException;
import com.veadan.folib.providers.search.SearchProvider;
import com.veadan.folib.services.ArtifactSearchService;
import com.veadan.folib.storage.search.SearchRequest;
import com.veadan.folib.storage.search.SearchResults;
import org.springframework.stereotype.Component;

/**
 * @author mtodorov
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
