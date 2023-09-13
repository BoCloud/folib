package com.veadan.folib.repositories;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
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
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.util.*;
import java.util.stream.Collectors;

@Repository
@Transactional
@Slf4j
public class ArtifactIdGroupRepository extends GremlinVertexRepository<ArtifactIdGroup> {

    @Inject
    ArtifactIdGroupAdapter adapter;
    @Inject
    ArtifactIdGroupQueries queries;
    @Inject
    ConfigurationManager configurationManager;
    @Inject
    @Lazy
    ArtifactAdapter artifactAdapter;

    @Override
    protected ArtifactIdGroupAdapter adapter() {
        return adapter;
    }

    public Page<ArtifactIdGroup> findMatching(String storageId,
                                              String repositoryId,
                                              Pageable page) {
        return queries.findMatching(storageId, repositoryId, page);
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

    public Boolean artifactsExists(Set<String> storageRepositoryIds,
                                   String artifactId,
                                   Collection<String> coordinateValues) {
        Set<String> artifactIdGroupIds = storageRepositoryIds.stream()
                .map(storageRepositoryId -> storageRepositoryId.split(":"))
                .map(storageRepositoryId -> new ArtifactIdGroupEntity(
                        storageRepositoryId[0],
                        storageRepositoryId[1], artifactId))
                .map(ArtifactIdGroup::getUuid)
                .collect(Collectors.toSet());

        return queries.artifactsExists(artifactIdGroupIds, coordinateValues);
    }

    public Long countArtifacts(Set<String> storageRepositoryIds,
                               String artifactId,
                               Collection<String> coordinateValues) {
        Set<String> artifactIdGroupIds = storageRepositoryIds.stream()
                .map(storageRepositoryId -> storageRepositoryId.split(":"))
                .map(storageRepositoryId -> new ArtifactIdGroupEntity(
                        storageRepositoryId[0],
                        storageRepositoryId[1], artifactId))
                .map(ArtifactIdGroup::getUuid)
                .collect(Collectors.toSet());

        return queries.countArtifacts(artifactIdGroupIds, coordinateValues);
    }

    public Long countArtifacts(String storageId,
                               String repositoryId,
                               String artifactId,
                               Collection<String> coordinateValues) {

        return queries.countArtifacts(Collections.singleton(new ArtifactIdGroupEntity(storageId, repositoryId, artifactId))
                        .stream()
                        .map(ArtifactIdGroup::getUuid)
                        .collect(Collectors.toSet()),
                coordinateValues);
    }

    public List<Artifact> findArtifacts(Set<String> storageRepositoryIds,
                                        String artifactId,
                                        Collection<String> coordinateValues,
                                        Long skip,
                                        Integer limit) {
        Set<String> artifactIdGroupIds = storageRepositoryIds.stream()
                .map(storageRepositoryId -> storageRepositoryId.split(":"))
                .map(storageRepositoryId -> new ArtifactIdGroupEntity(
                        storageRepositoryId[0],
                        storageRepositoryId[1], artifactId))
                .map(ArtifactIdGroup::getUuid)
                .collect(Collectors.toSet());

        return queries.findArtifacts(artifactIdGroupIds, coordinateValues, skip, limit);
    }

    public List<Artifact> findArtifacts(String storageId,
                                        String repositoryId,
                                        String artifactId,
                                        Collection<String> coordinateValues,
                                        Long skip,
                                        Integer limit) {
        return queries.findArtifacts(Collections.singleton(new ArtifactIdGroupEntity(storageId, repositoryId, artifactId))
                        .stream()
                        .map(ArtifactIdGroup::getUuid)
                        .collect(Collectors.toSet()),
                coordinateValues,
                skip,
                limit);
    }

    public List<Artifact> findArtifactsGremlin(String storageId,
                                               String repositoryId,
                                               String artifactId,
                                               Collection<String> coordinateValues,
                                               Long skip,
                                               Integer limit,
                                               Boolean useLimit) {
        if (Boolean.FALSE.equals(useLimit)) {
            skip = 0L;
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            long startTime = System.currentTimeMillis();
            Long count = commonSearchCountArtifacts(storageId, repositoryId, artifactId, coordinateValues);
            log.info("ArtifactIdGroup [{}] commonSearchCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            if (Objects.isNull(count) || skip.equals(count)) {
                startTime = System.currentTimeMillis();
                count = commonCountArtifacts(storageId, repositoryId, artifactId, coordinateValues);
                log.info("ArtifactIdGroup [{}] commonCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            }
            if (count > 0L) {
                limit = count.intValue();
            }
        }
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long startTime = System.currentTimeMillis();
        List<Artifact> artifactList = commonFindArtifacts(storageId, repositoryId, artifactId, coordinateValues).map(artifactAdapter.fold(Optional.ofNullable(repository)
                .map(com.veadan.folib.storage.repository.Repository::getLayout)
                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).range(skip, limit).toList();
        if (CollectionUtils.isEmpty(artifactList)) {
            artifactList = Collections.emptyList();
        }
        log.info("FindArtifactsGremlin storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, artifactId, coordinateValues, skip, limit, useLimit, artifactList.size(), System.currentTimeMillis() - startTime);
        return artifactList;
    }

    public long commonCountArtifacts(String storageId,
                                     String repositoryId,
                                     String artifactId,
                                     Collection<String> coordinateValues) {
        return commonFindArtifacts(storageId, repositoryId, artifactId, coordinateValues).count().tryNext().orElse(0L);
    }

    public Boolean commonArtifactsExists(String storageId,
                                         String repositoryId,
                                         String artifactId,
                                         Collection<String> coordinateValues) {
        return commonCountArtifacts(storageId, repositoryId, artifactId, coordinateValues) > 0L;
    }

    private EntityTraversal<Vertex, Vertex> commonFindArtifacts(String storageId,
                                                                String repositoryId,
                                                                String artifactId,
                                                                Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, Vertex> t = g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup.getUuid()).outE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS).inV()
                .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.UUID, Text.textPrefix(artifactIdGroup.getUuid()));
        if (CollectionUtils.isNotEmpty(coordinateValues)) {
            for (String coordinateValue : coordinateValues) {
                t = t.has(Properties.UUID, Text.textContains("." + coordinateValue));
            }
        }
        return t;
    }

    /**
     * 统计 走搜索引擎 数据时效性稍微落后
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
                                           Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, Vertex> t = g().V()
                .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId);
        String regex = "(%s/)", suffix= "";
        //(folib-common-taobao-npm-vue)(.*tgz.*)
        regex = String.format(regex, artifactIdGroup.getUuid());
        if (CollectionUtils.isNotEmpty(coordinateValues)) {
            for (String coordinateValue : coordinateValues) {
                suffix = "(.*%s.*)";
                suffix = String.format(suffix, coordinateValue);
                regex = regex + suffix;
            }
        }
        t.has(Properties.UUID, Text.textRegex(regex));
        return t.count().tryNext().orElse(0L);
    }

    public ArtifactIdGroup findByArtifactIdGroup(String artifactIdGroup) {
        return g().V().hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup).map(adapter.artifactIdGroupFold()).tryNext().orElse(null);
    }
}

@Repository
interface ArtifactIdGroupQueries
        extends org.springframework.data.repository.Repository<ArtifactIdGroup, String> {
    @Query(value = "MATCH (aig:`ArtifactIdGroup`) " +
            "WHERE aig.storageId=$storageId AND aig.repositoryId=$repositoryId " +
            "WITH aig " +
            "OPTIONAL MATCH (aig)-[r0:ArtifactGroupHasArtifacts]->(artifact:Artifact)-[r1]->(genericCoordinates:GenericArtifactCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (aig:`ArtifactIdGroup`) " +
                    "WHERE aig.storageId=$storageId AND aig.repositoryId=$repositoryId " +
                    "RETURN count(aig)")
    Page<ArtifactIdGroup> findMatching(@Param("storageId") String storageId,
                                       @Param("repositoryId") String repositoryId,
                                       Pageable page);

    //TODO: `OPTIONAL` is workaround for https://github.com/opencypher/cypher-for-gremlin/issues/342 
    @Query("OPTIONAL MATCH (aig:`ArtifactIdGroup`) " +
            "WHERE aig.uuid IN $artifactIdGroupIds " +
            "WITH aig " +
            "MATCH (aig)-[r0:ArtifactGroupHasArtifacts]->(artifact:Artifact)-[r1]->(genericCoordinates:GenericArtifactCoordinates)<-[r2]-(layoutCoordinates) " +
            "UNWIND keys(genericCoordinates) AS coordinate " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, coordinate " +
            "WHERE coordinate STARTS WITH 'coordinates.' AND genericCoordinates[coordinate] IN $coordinateValues " +
            "RETURN exists(artifact.uuid) LIMIT 1")
    Boolean artifactsExists(@Param("artifactIdGroupIds") Set<String> artifactIdGroupIds,
                            @Param("coordinateValues") Collection<String> coordinateValues);

    //TODO: `OPTIONAL` is workaround for https://github.com/opencypher/cypher-for-gremlin/issues/342
    @Query("OPTIONAL MATCH (aig:`ArtifactIdGroup`) " +
            "WHERE aig.uuid IN $artifactIdGroupIds " +
            "WITH aig " +
            "MATCH (aig)-[r0:ArtifactGroupHasArtifacts]->(artifact:Artifact)-[r1]->(genericCoordinates:GenericArtifactCoordinates)<-[r2]-(layoutCoordinates) " +
            "UNWIND keys(genericCoordinates) AS coordinate " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, coordinate " +
            "WHERE coordinate STARTS WITH 'coordinates.' AND genericCoordinates[coordinate] IN $coordinateValues " +
            "RETURN count(artifact)")
    Long countArtifacts(@Param("artifactIdGroupIds") Set<String> artifactIdGroupIds,
                        @Param("coordinateValues") Collection<String> coordinateValues);

    //TODO: `OPTIONAL` is workaround for https://github.com/opencypher/cypher-for-gremlin/issues/342
    @Query("OPTIONAL MATCH (aig:`ArtifactIdGroup`) " +
            "WHERE aig.uuid IN $artifactIdGroupIds " +
            "WITH aig " +
            "MATCH (aig)-[r0:ArtifactGroupHasArtifacts]->(artifact:Artifact)-[r1]->(genericCoordinates:GenericArtifactCoordinates)<-[r2]-(layoutCoordinates) " +
            "UNWIND keys(genericCoordinates) AS coordinate " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, coordinate " +
            "WHERE coordinate STARTS WITH 'coordinates.' AND genericCoordinates[coordinate] IN $coordinateValues " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates,  r4, tag " +
            "ORDER BY aig.name, genericCoordinates.version " +
            "SKIP $skip LIMIT $limit")
    List<Artifact> findArtifacts(@Param("artifactIdGroupIds") Set<String> artifactIdGroupIds,
                                 @Param("coordinateValues") Collection<String> coordinateValues,
                                 @Param("skip") Long skip,
                                 @Param("limit") Integer limit);

}
