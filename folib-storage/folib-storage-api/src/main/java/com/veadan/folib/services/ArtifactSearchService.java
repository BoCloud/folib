package com.veadan.folib.services;

import com.veadan.folib.providers.search.SearchException;
import com.veadan.folib.storage.search.SearchRequest;
import com.veadan.folib.storage.search.SearchResults;

/**
 * @author mtodorov
 */
public interface ArtifactSearchService
{

    SearchResults search(SearchRequest searchRequest)
            throws SearchException;

    boolean contains(SearchRequest searchRequest)
            throws SearchException;

}
