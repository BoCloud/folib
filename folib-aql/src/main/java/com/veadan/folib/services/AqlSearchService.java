package com.veadan.folib.services;

import java.io.IOException;

import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.domain.ArtifactEntity;

public interface AqlSearchService
{

    public SearchResults search(Selector<ArtifactEntity> selector)
        throws IOException;

}
