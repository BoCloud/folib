package com.veadan.folib.services.impl;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.lang.reflect.UndeclaredThrowableException;

@Service
@Transactional
public class ArtifactServiceImpl implements ArtifactService {

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private JanusGraph janusGraph;

    @Override
    public void saveOrUpdateArtifact(Artifact artifact) {
        Graph g = janusGraph.tx().createThreadedTx();
        try {
            artifact.setLastUpdated(LocalDateTimeInstance.now());
            artifactRepository.save(() -> g.traversal(EntityTraversalSource.class), artifact);
            g.tx().commit();
        } catch (Exception e) {
            g.tx().rollback();
            throw new UndeclaredThrowableException(e);
        } finally {
            g.tx().close();
        }
    }
}
