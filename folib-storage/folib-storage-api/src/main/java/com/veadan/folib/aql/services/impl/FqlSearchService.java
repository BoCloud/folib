package com.veadan.folib.aql.services.impl;

import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.aql.services.AqlSearchService;
import com.veadan.folib.storage.search.SearchResults;
import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;

@Component
@Transactional
public class FqlSearchService extends GremlinVertexRepository<Artifact> implements AqlSearchService {
    @Inject
    ArtifactAdapter artifactAdapter;

    @Inject
    ArtifactRepository artifactRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Inject
    private SnippetGenerator snippetGenerator;

    @Override
    public SearchResults search(Selector<ArtifactEntity> selector) throws IOException {

        return null;
    }

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }

}
