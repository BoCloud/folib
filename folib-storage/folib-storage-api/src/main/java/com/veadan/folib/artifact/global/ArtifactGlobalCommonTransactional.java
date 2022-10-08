package com.veadan.folib.artifact.global;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Objects;

/**
 * 制品顶点全局通用事务处理，与图数据库交互，更新数据
 *
 * @author leipenghui
 * @date 2022/9/27
 **/
@Component
public class ArtifactGlobalCommonTransactional {

    private static final Logger logger = LoggerFactory.getLogger(ArtifactGlobalCommonTransactional.class);

    @Inject
    private ArtifactRepository artifactEntityRepository;

    @Inject
    private JanusGraph janusGraph;

    /**
     * 处理制品顶点
     * @param artifact 制品
     */
    public void handleArtifact(final Artifact artifact) {
        if (Objects.isNull(artifact)) {
            return;
        }
        try {
            handleTransactional(artifact);
        } catch (Throwable e) {
            logger.error("制品顶点全局通用事务处理失败，类名：{}，制品：{}",
                    ArtifactGlobalCommonTransactional.this.getClass().getSimpleName(),
                    artifact,
                    e);
        }
    }

    private void handleTransactional(Artifact artifact) {
        Graph g = janusGraph.tx().createThreadedTx();
        try {
            artifact.setLastUpdated(LocalDateTimeInstance.now());
            artifactEntityRepository.merge(() -> g.traversal(EntityTraversalSource.class), artifact);
            g.tx().commit();
        } catch (Throwable e) {
            g.tx().rollback();
            throw new UndeclaredThrowableException(e);
        } finally {
            g.tx().close();
        }
    }
}
