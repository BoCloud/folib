package com.folib.aql.services.impl;

import com.folib.data.criteria.Selector;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.aql.services.AqlSearchService;
import com.folib.storage.search.SearchResults;
import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;

@Component
@Transactional
public class FqlSearchServiceImpl extends GremlinVertexRepository<Artifact> implements AqlSearchService {
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
