package com.folib.aql.services;

import java.io.IOException;

import com.folib.data.criteria.Selector;
import com.folib.storage.search.SearchResults;
import com.folib.domain.ArtifactEntity;

public interface AqlSearchService
{

    public SearchResults search(Selector<ArtifactEntity> selector)
        throws IOException;

}
