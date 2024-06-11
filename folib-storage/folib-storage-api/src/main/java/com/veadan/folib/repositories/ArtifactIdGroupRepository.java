package com.veadan.folib.repositories;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.ArtifactIdGroupAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Repository
@Transactional
@Slf4j
public class ArtifactIdGroupRepository extends GremlinVertexRepository<ArtifactIdGroup> {

    @Inject
    ArtifactIdGroupAdapter adapter;
    @Inject
    ConfigurationManager configurationManager;
    @Inject
    @Lazy
    ArtifactAdapter artifactAdapter;
    @Inject
    DistributedLockComponent distributedLockComponent;

    public void saveOrUpdate(ArtifactIdGroup artifactIdGroup) {
        if (distributedLockComponent.lock(artifactIdGroup.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    merge(artifactIdGroup);
                } catch (Exception ex) {
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle artifactIdGroup [{}] catch error", artifactIdGroup.getUuid());
                        return;
                    }
                    log.error("Handle artifactIdGroup [{}] error [{}]", artifactIdGroup.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
            } finally {
                distributedLockComponent.unLock(artifactIdGroup.getUuid());
            }
        } else {
            log.warn("Handle artifactIdGroup [{}] was not get lock", artifactIdGroup.getUuid());
        }
    }

    @Override
    protected ArtifactIdGroupAdapter adapter() {
        return adapter;
    }

    public Page<ArtifactIdGroup> findMatching(String storageId,
                                              String repositoryId,
                                              Pageable page) {
        Long count = commonArtifactIdGroupPage(storageId, repositoryId).count().tryNext().orElse(0L);
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long low = page.getPageNumber() * page.getPageSize();
        long high = (page.getPageNumber() + 1) * page.getPageSize();
        List<ArtifactIdGroup> artifactIdGroupList = commonArtifactIdGroupPage(storageId, repositoryId)
                .map(adapter.fold(Optional.ofNullable(repository)
                                .map(com.veadan.folib.storage.repository.Repository::getLayout)
                                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass),
                        Optional.empty())).range(low, high).toList();
        return new PageImpl<>(artifactIdGroupList, page, count);
    }

    private EntityTraversal<Vertex, Vertex> commonArtifactIdGroupPage(String storageId, String repositoryId) {
        return g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has(Properties.STORAGE_ID, storageId)
                .has(Properties.REPOSITORY_ID, repositoryId);
    }

    public Optional<ArtifactIdGroup> findAllArtifactsInGroup(String storageId,
                                                             String repositoryId,
                                                             String artifactId) {
        return findArtifactsGroupWithTag(storageId, repositoryId, artifactId, Optional.empty());
    }

    public Optional<ArtifactIdGroup> findArtifactsGroupWithTag(String storageId,
                                                               String repositoryId,
                                                               String artifactId,
                                                               Optional<ArtifactTag> tag) {
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);

        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, ArtifactIdGroup> t = g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has("uuid", artifactIdGroup.getUuid())
                .map(adapter.fold(Optional.ofNullable(repository)
                                .map(com.veadan.folib.storage.repository.Repository::getLayout)
                                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass),
                        tag));
        if (!t.hasNext()) {
            return Optional.empty();
        }

        return Optional.of(t.next());
    }

    public Optional<ArtifactIdGroup> findArtifactGroupWithTag(String storageId,
                                                              String repositoryId,
                                                              String artifactId,
                                                              Optional<ArtifactTag> tag) {
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, ArtifactIdGroup> t = g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has("uuid", artifactIdGroup.getUuid())
                .map(adapter.artifactIdGroupFold());
        if (!t.hasNext()) {
            return Optional.empty();
        }

        return Optional.of(t.next());
    }

    public List<Artifact> findArtifactsGremlin(String storageId,
                                               String repositoryId,
                                               String artifactId,
                                               Boolean useArtifactName,
                                               Collection<String> coordinateValues,
                                               Long skip,
                                               Integer limit,
                                               Boolean useLimit) {
        if (Boolean.FALSE.equals(useLimit)) {
            skip = 0L;
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            long startTime = System.currentTimeMillis();
            Long count = commonSearchCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues);
            log.info("ArtifactIdGroup [{}] commonSearchCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            if (Objects.isNull(count) || skip.equals(count)) {
                startTime = System.currentTimeMillis();
                count = commonCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues);
                log.info("ArtifactIdGroup [{}] commonCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            }
            if (count > 0L) {
                limit = count.intValue();
            }
        }
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long startTime = System.currentTimeMillis();
        List<Artifact> artifactList = commonFindArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues).order().by(Properties.CREATED, Order.asc).map(artifactAdapter.fold(Optional.ofNullable(repository)
                .map(com.veadan.folib.storage.repository.Repository::getLayout)
                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).range(skip, limit).toList();
        if (CollectionUtils.isEmpty(artifactList)) {
            artifactList = Collections.emptyList();
        }
        log.debug("FindArtifactsGremlin storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, artifactId, coordinateValues, skip, limit, useLimit, artifactList.size(), System.currentTimeMillis() - startTime);
        return artifactList;
    }

    public long commonCountArtifacts(String storageId,
                                     String repositoryId,
                                     String artifactId,
                                     Boolean useArtifactName,
                                     Collection<String> coordinateValues) {
        return commonFindArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues).count().tryNext().orElse(0L);
    }

    public Boolean commonArtifactsExists(String storageId,
                                         String repositoryId,
                                         String artifactId,
                                         Boolean useArtifactName,
                                         Collection<String> coordinateValues) {
        return commonCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues) > 0L;
    }

    private EntityTraversal<Vertex, Vertex> commonFindArtifacts(String storageId,
                                                                String repositoryId,
                                                                String artifactId,
                                                                Boolean useArtifactName,
                                                                Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        EntityTraversal<Vertex, Vertex> t = null;
        if (Boolean.TRUE.equals(useArtifactName)) {
            t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.ARTIFACT_NAME, Text.textPrefix(artifactId));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, true);
            }
        } else {
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            t = g().V()
                    .hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup.getUuid()).outE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS).inV()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.UUID, Text.textPrefix(artifactIdGroup.getUuid()));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, false);
            }
        }
        return t;
    }

    /**
     * 统计 走搜索引擎
     *
     * @param storageId        storageId
     * @param repositoryId     repositoryId
     * @param artifactId       artifactId
     * @param coordinateValues coordinateValues
     * @return 个数
     */
    public Long commonSearchCountArtifacts(String storageId,
                                           String repositoryId,
                                           String artifactId,
                                           Boolean useArtifactName,
                                           Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        if (Boolean.TRUE.equals(useArtifactName)) {
            EntityTraversal<Vertex, Vertex> t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId)
                    .has(Properties.ARTIFACT_NAME, Text.textPrefix(artifactId));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, true);
            }
            return t.count().tryNext().orElse(0L);
        } else {
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            EntityTraversal<Vertex, Vertex> t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId);
            String regex = "(%s/)", suffix = "";
            //(folib-common-taobao-npm-vue)(.*tgz.*)
            regex = String.format(regex, artifactIdGroup.getUuid());
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                suffix = "(.*.(%s))";
                suffix = String.format(suffix, String.join("|", coordinateValues));
                regex = regex + suffix;
            }
            t.has(Properties.UUID, Text.textRegex(regex));
            return t.count().tryNext().orElse(0L);
        }
    }

    public ArtifactIdGroup findByArtifactIdGroup(String artifactIdGroup) {
        return g().V().hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup).map(adapter.artifactIdGroupFold()).tryNext().orElse(null);
    }

    private void handleCoordinateValues(EntityTraversal<Vertex, Vertex> t, Collection<String> coordinateValues, Boolean useArtifactName) {
        String s = ".*.(%s)";
        final String regex = String.format(s, String.join("|", coordinateValues));
        if (Boolean.TRUE.equals(useArtifactName)) {
            t.has(Properties.ARTIFACT_NAME, Text.textRegex(regex));
        } else {
            t.has(Properties.UUID, Text.textRegex(regex));
        }
    }
}