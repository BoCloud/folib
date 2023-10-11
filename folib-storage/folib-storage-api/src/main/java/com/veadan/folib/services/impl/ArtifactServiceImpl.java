package com.veadan.folib.services.impl;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.util.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.lang.reflect.UndeclaredThrowableException;

@Slf4j
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
            artifact.setUpdatedBy(UserUtils.getUsername());
            artifactRepository.save(() -> g.traversal(EntityTraversalSource.class), artifact);
            if (g.tx().isOpen()) {
                g.tx().commit();
            }
        } catch (Exception ex) {
            if (g.tx().isOpen()) {
                g.tx().rollback();
            }
            String realMessage = CommonUtils.getRealMessage(ex);
            log.warn("[{}] [{}] saveOrUpdateArtifact error [{}]",
                    this.getClass().getSimpleName(), artifact.getUuid(), realMessage);
            if (CommonUtils.catchException(realMessage)) {
                log.warn("[{}] [{}] saveOrUpdateArtifact catch error",
                        this.getClass().getSimpleName(), artifact.getUuid());
                return;
            }
            throw new UndeclaredThrowableException(ex);
        } finally {
            g.tx().close();
        }
    }
}
