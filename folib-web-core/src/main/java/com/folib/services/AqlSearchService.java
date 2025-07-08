package com.folib.services;

import com.folib.data.criteria.Selector;
import com.folib.domain.ArtifactEntity;
import com.folib.storage.search.SearchResults;

import java.io.IOException;

public interface AqlSearchService
{

    public SearchResults search(Selector<ArtifactEntity> selector)
        throws IOException;

}
