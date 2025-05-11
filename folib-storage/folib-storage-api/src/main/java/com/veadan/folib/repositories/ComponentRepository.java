package com.veadan.folib.repositories;

import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Component;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.ComponentAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.util.CommonUtils;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.JanusGraph;
import org.janusgraph.core.attribute.Text;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 组件顶点图数据交互
 *
 * @author leipenghui
 **/
@Slf4j
@Repository
@Transactional
public class ComponentRepository extends GremlinVertexRepository<Component> {

    @Inject
    private ComponentAdapter componentAdapter;

    @Inject
    private ArtifactAdapter artifactAdapter;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private JanusGraph janusGraph;

    @Inject
    DistributedLockComponent distributedLockComponent;

    @Override
    protected ComponentAdapter adapter() {
        return componentAdapter;
    }

    public void saveOrUpdate(Component component) {
        if (distributedLockComponent.lock(component.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    merge(component);
                } catch (Exception ex) {
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle component [{}] catch error", component.getUuid());
                        return;
                    }
                    log.error("Handle component [{}] error [{}]", component.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
            } finally {
                distributedLockComponent.unLock(component.getUuid());
            }
        } else {
            log.warn("Handle component [{}] was not get lock", component.getUuid());
        }
    }

    public Page<Component> queryComponentPage(Pageable pagination, String name, String groupId, String version, String fileName) {
        Long count = commonBuildEntityTraversal(name, groupId, version, fileName).count().tryNext().orElse(0L);
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Component> componentList = commonBuildEntityTraversal(name, groupId, version, fileName)
                .range(low, high)
                .map(componentAdapter.fold()).toList();
        return new PageImpl<Component>(EntityTraversalUtils.reduceHierarchy(componentList), pagination, count);
    }

    public Page<Component> queryComponentPageByArtifact(Pageable pagination, String artifactPath,
                                                        String fileName) {
        Long count = commonBuildEntityTraversalByArtifact(artifactPath, fileName).count().tryNext().orElse(0L);
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Component> componentList = commonBuildEntityTraversalByArtifact(artifactPath, fileName)
                .range(low, high)
                .map(componentAdapter.fold()).toList();
        return new PageImpl<Component>(EntityTraversalUtils.reduceHierarchy(componentList), pagination, count);
    }

    /**
     * 构建公共图查询
     *
     * @param fileName fileName
     * @return 公共图查询
     */
    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversal(String name, String groupId, String version, String fileName) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.COMPONENT).has(Properties.CREATED, P.gt(0));
        if (StringUtils.isNotBlank(name)) {
            entityTraversal = entityTraversal.has(Properties.NAME, Text.textContains(name));
        }
        if (StringUtils.isNotBlank(groupId)) {
            entityTraversal = entityTraversal.has(Properties.GROUP_ID, Text.textContains(groupId));
        }
        if (StringUtils.isNotBlank(version)) {
            entityTraversal = entityTraversal.has(Properties.VERSION, Text.textContains(version));
        }
        if (StringUtils.isNotBlank(fileName)) {
            entityTraversal = entityTraversal.has(Properties.FILE_NAME, Text.textContains(fileName));
        }
        return entityTraversal;
    }

    /**
     * 构建公共图查询
     *
     * @param fileName fileName
     * @return 公共图查询
     */
    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversalByArtifact(String artifactPath, String fileName) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.UUID, artifactPath).outE(Edges.ARTIFACT_HAS_COMPONENTS).inV();
        if (StringUtils.isNotBlank(fileName)) {
            entityTraversal = entityTraversal.has(Properties.FILE_NAME, Text.textContains(fileName));
        }
        return entityTraversal;
    }
}

//@Repository
//interface ComponentQueries
//        extends org.springframework.data.repository.Repository<Component, String> {
//
//}