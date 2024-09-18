package com.veadan.folib.indexer;

import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.storage.repository.Repository;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author huayanjun
 * @since 2024-09-07 15:34
 */
@Component
public class ArtifactorySearch extends GremlinVertexRepository<Artifact> {


    @Resource
    ArtifactAdapter artifactAdapter;

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    public List<Artifact> findReleaseByDistribution(String distribution, Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT)
                .has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId()).map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get("extension").equals(DebianConstant.PACKAGE_EXTENSION))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.DISTRIBUTION).equals(distribution))
                .collect(Collectors.toList());

    }

    public List<Artifact> findByDistributionAndComponent(String distribution, String component, Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT)
                .has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId())
                .map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.DISTRIBUTION).equals(distribution))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.COMPONENT).equals(component))
                .collect(Collectors.toList());
    }

    public List<Artifact> findAllPackage(Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId()).map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.EXTENSION).equals(DebianConstant.PACKAGE_EXTENSION))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.NAME).equals("Packages"))
                .collect(Collectors.toList());
    }


}
