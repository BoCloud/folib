package com.veadan.folib.services.impl;

import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import com.veadan.folib.dependency.snippet.SnippetGenerator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.AqlSearchService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.net.URL;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Component
@Transactional
public class FqlSearchService extends GremlinVertexRepository<Artifact> implements AqlSearchService{
    @Inject
    ArtifactAdapter artifactAdapter;

    @Inject
    ArtifactRepository  artifactRepository;

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


    public SearchResults artfactQuery(String artifactName,
                                       String storageId,
                                       String repositoryId,
                                       int limit,int page) throws IOException {

        Pageable pageable = PageRequest.of(page, limit).next();
        Page<Artifact> artifacts = null;
        if(storageId == null||repositoryId==null){
             artifacts= artifactRepository.findMatching1(artifactName,pageable);

        }else {
            artifacts= artifactRepository.findMatching2(artifactName,storageId,repositoryId,pageable);
        }
        List<Artifact> artifactEntityList = artifacts.getContent();
        SearchResults result = new SearchResults();
        for (Artifact artifact:artifactEntityList){
            SearchResult r = new SearchResult();
            result.getResults().add(r);

            r.setStorageId(artifact.getStorageId());
            r.setRepositoryId(artifact.getRepositoryId());
            r.setArtifactCoordinates(artifact.getArtifactCoordinates());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(),
                    artifact.getRepositoryId(),
                    artifact.getArtifactPath());

            r.setChecksums(artifact.getChecksums());
            r.setSizeInBytes(artifact.getSizeInBytes());

            Repository repository = repositoryPath.getRepository();

            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());

            List<CodeSnippet> snippets = snippetGenerator.generateSnippets(repository.getLayout(),
                    artifact.getArtifactCoordinates());
            r.setSnippets(snippets);
        }
        return result;
    }
}
