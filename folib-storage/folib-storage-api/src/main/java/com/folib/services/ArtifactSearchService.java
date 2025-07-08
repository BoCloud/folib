package com.folib.services;

import com.folib.providers.search.SearchException;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResults;

/**
 * @author veadan
 */
public interface ArtifactSearchService
{

    SearchResults search(SearchRequest searchRequest)
            throws SearchException;

    boolean contains(SearchRequest searchRequest)
            throws SearchException;

}
