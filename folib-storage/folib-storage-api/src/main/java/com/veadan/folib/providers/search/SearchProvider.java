package com.veadan.folib.providers.search;

import com.veadan.folib.storage.search.SearchRequest;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;

/**
 * @author Veadan
 */
public interface SearchProvider
{
    String getAlias();

    SearchResults search(SearchRequest searchRequest)
            throws SearchException;

    /**
     * This method should be used only for exact queries where preliminary results
     * have found a list of matches. It should use full artifact coordinates,
     * as the purpose of this is so that more details such as the dependency snippets
     * could be provided in the result.
     *
     * @param searchRequest
     * @return
     */
    SearchResult findExact(SearchRequest searchRequest);

    boolean contains(SearchRequest searchRequest)
            throws SearchException;

}
