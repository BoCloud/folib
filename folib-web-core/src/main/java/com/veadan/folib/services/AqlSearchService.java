package com.veadan.folib.services;

import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.storage.search.SearchResults;

import java.io.IOException;

public interface AqlSearchService
{

    public SearchResults search(Selector<ArtifactEntity> selector)
        throws IOException;

}
