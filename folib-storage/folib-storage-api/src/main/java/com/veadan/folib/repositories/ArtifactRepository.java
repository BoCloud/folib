package com.veadan.folib.repositories;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilityArtifactDomain;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.dsl.__;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.util.LocalCacheUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;


@Repository
@Transactional
public class ArtifactRepository extends GremlinVertexRepository<Artifact> {
    //查找标记
    @Inject
    ArtifactAdapter artifactAdapter;
    @Inject
    ArtifactEntityQueries queries;
    @Inject
    ConfigurationManager configurationManager;

    @Override
    protected ArtifactAdapter adapter() {
        return artifactAdapter;
    }

    public List<Artifact> findByPathLike(String storageId,
                                         String repositoryId,
                                         String path) {
        return EntityTraversalUtils.reduceHierarchy(queries.findByPathLike(storageId, repositoryId, path));
    }

    public Page<Artifact> findMatching(Integer lastAccessedTimeInDays,
                                       Long minSizeInBytes,
                                       Pageable pagination) {
        LocalDateTime date = Optional.ofNullable(lastAccessedTimeInDays)
                .map(v -> LocalDateTime.now().minusDays(lastAccessedTimeInDays))
                .orElse(null);
        return findMatching(date, minSizeInBytes, pagination);
    }

    public Page<Artifact> findMatching(LocalDateTime lastAccessedDate,
                                       Long minSizeInBytes,
                                       Pageable pagination) {
        Page<Artifact> result = queries.findMatching(lastAccessedDate, minSizeInBytes, pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching1(String artifactName,
                                        Pageable pagination) {
        Page<Artifact> result = queries.findMatching1(artifactName, pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching2(String artifactName,
                                        String storageId,
                                        String repositoryId,
                                        Pageable pagination) {
        Page<Artifact> result = queries.findMatching2(artifactName, storageId, repositoryId, pagination);
        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Boolean artifactEntityExists(String storageId,
                                        String repositoryId,
                                        String path) {
        return Optional.ofNullable(queries.artifactEntityExists(storageId, repositoryId, path)).orElse(Boolean.FALSE);
    }

    public Page<Artifact> findMatchingByIndex(Pageable pagination, Boolean regex, String artifactName,
                                              String metadataSearch,
                                              String storageId,
                                              String repositoryId,
                                              String beginDate,
                                              String endDate,
                                              String sortField,
                                              String sortOrder) {
        com.veadan.folib.storage.repository.Repository repository = null;
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            repository = configurationManager.getRepository(storageId, repositoryId);
        }
        Long count = buildEntityTraversal(regex, artifactName, metadataSearch, storageId, repositoryId, beginDate, endDate, sortField, sortOrder).count().tryNext().orElse(0L);
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();


        List<Artifact> artifactList = buildEntityTraversal(regex, artifactName, metadataSearch, storageId, repositoryId, beginDate, endDate, sortField, sortOrder)
                .range(low, high)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return new PageImpl<>(artifactList, pagination, count);
    }

    public List<Artifact> findMatchingByVulnerabilityUuid(String vulnerabilityUuid,
                                                          String storageId,
                                                          String repositoryId) {
        com.veadan.folib.storage.repository.Repository repository = null;
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            repository = configurationManager.getRepository(storageId, repositoryId);
        }
        List<Artifact> artifactList = buildEntityTraversalByVulnerabilityUuid(vulnerabilityUuid, storageId, repositoryId)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return EntityTraversalUtils.reduceHierarchy(artifactList);
    }

    public Long countByStorageIdAndRepositoryId(String storageId, String repositoryId) {
        return g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId).count().tryNext().orElse(0L);
    }

    public Map<Object, Object> countArtifactByStorageIdAndRepositoryId(String storageId, String repositoryId) {
        String key = "countArtifactByStorageIdAndRepositoryId-%s-%s";
        key = String.format(key, storageId, repositoryId);
        String cacheValue = LocalCacheUtils.get(key);
        if (StringUtils.isNotBlank(cacheValue)) {
            return Maps.newHashMap(JSONObject.parseObject(cacheValue));
        }
        EntityTraversal<Vertex, Map<Object, Object>> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID, storageId).has(Properties.REPOSITORY_ID, repositoryId)
                .properties(Properties.DOWNLOAD_COUNT, Properties.DEPENDENCY_COUNT).
                        group().by(__.key()).by(__.value().sum());
        Map<Object, Object> map = entityTraversal.tryNext().orElse(Maps.newHashMap());
        LocalCacheUtils.put(key, JSONObject.toJSONString(map), 3600);
        return map;
    }

    public List<VulnerabilityArtifactDomain> findMatchingHasVulnerabilityByStorageIdsAndLevels(List<String> storageIdList, Set<String> levels) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.VULNERABILITY).has(Properties.VULNERABILITY_PLATFORM_NAME, P.within(VulnerabilityPlatformEnum.values()))
                .has(Properties.HIGHEST_SEVERITY_TEXT, P.within(levels)).as("v");
        if (CollectionUtils.isNotEmpty(storageIdList)) {
            entityTraversal = entityTraversal.inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV()
                    .has(Properties.STORAGE_ID, P.within(storageIdList));
        } else {
            entityTraversal = entityTraversal.inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV();
        }
        List<VulnerabilityArtifactDomain> artifactList = entityTraversal.map(artifactAdapter.vulnerabilityFold()).toList();
        return artifactList;
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversal(Boolean regex, String artifactName,
                                                                 String metadataSearch,
                                                                 String storageId,
                                                                 String repositoryId,
                                                                 String beginDate,
                                                                 String endDate,
                                                                 String sortField,
                                                                 String sortOrder) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT);
        if (StringUtils.isNotBlank(artifactName)) {
            if (Boolean.TRUE.equals(regex)) {
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textRegex(artifactName));
            } else {
                entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(artifactName));
            }
        }
        if (StringUtils.isNotBlank(metadataSearch)) {
            entityTraversal = entityTraversal.has(Properties.METADATA, Text.textContains(metadataSearch));
        }
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has(Properties.STORAGE_ID, storageId);
        }
        if (StringUtils.isNotBlank(repositoryId)) {
            entityTraversal = entityTraversal.has(Properties.REPOSITORY_ID, repositoryId);
        }
        if (StringUtils.isNotBlank(sortField) && StringUtils.isNotBlank(sortOrder)) {
            entityTraversal = entityTraversal.order().by(sortField, Order.valueOf(sortOrder));
        }
        if (StringUtils.isNotBlank(beginDate) && StringUtils.isNotBlank(endDate)) {
            LocalDateTime beginLocalDateTime = DateUtil.parseLocalDateTime(beginDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
            LocalDateTime endLocalDateTime = DateUtil.parseLocalDateTime(endDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
            Long begin = EntityTraversalUtils.toLong(beginLocalDateTime);
            Long end = EntityTraversalUtils.toLong(endLocalDateTime);
            entityTraversal = entityTraversal.has(Properties.CREATED, P.between(begin, end));
        }
        return entityTraversal;
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversalByVulnerabilityUuid(String vulnerabilityUuid,
                                                                                    String storageId,
                                                                                    String repositoryId) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.VULNERABILITY)
                .has("uuid", vulnerabilityUuid).inE(Edges.ARTIFACT_HAS_VULNERABILITIES).outV();
        if (StringUtils.isNotBlank(storageId)) {
            entityTraversal = entityTraversal.has("storageId", storageId);
        }
        if (StringUtils.isNotBlank(repositoryId)) {
            entityTraversal = entityTraversal.has("repositoryId", repositoryId);
        }
        return entityTraversal;
    }

    public Boolean artifactExists(String storageId,
                                  String repositoryId,
                                  String path) {
        EntityTraversal<Vertex, Vertex> t = g().V()
                .hasLabel(Vertices.GENERIC_ARTIFACT_COORDINATES)
                .has("uuid", path)
                .inE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                .otherV()
                .hasLabel(Vertices.ARTIFACT)
                .has("storageId", storageId)
                .has("repositoryId", repositoryId)
                .has("artifactFileExists", true);
        return t.hasNext();
    }

    public Artifact findOneArtifact(String storageId,
                                    String repositoryId,
                                    String path) {

        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);

        EntityTraversal<Vertex, Artifact> t = g().V()
                .hasLabel(Vertices.GENERIC_ARTIFACT_COORDINATES)
                .has("uuid", path)
                .inE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                .otherV()
                .hasLabel(Vertices.ARTIFACT)
                .has("storageId", storageId)
                .has("repositoryId", repositoryId)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).range(0, 1);
        if (!t.hasNext()) {
            return null;
        }
        return t.next();
    }

}

@Repository
interface ArtifactEntityQueries extends org.springframework.data.repository.Repository<Artifact, String> {

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE genericCoordinates.uuid STARTS WITH $path AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag")
    List<Artifact> findByPathLike(@Param("storageId") String storageId,
                                  @Param("repositoryId") String repositoryId,
                                  @Param("path") String path);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) AND artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) AND artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching(@Param("lastAccessedDate") LocalDateTime lastAccessedDate,
                                @Param("minSizeInBytes") Long minSizeInBytes,
                                Pageable page);

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE genericCoordinates.uuid=$path AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "RETURN exists(artifact.uuid)")
    Boolean artifactEntityExists(@Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 @Param("path") String path);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid CONTAINS  $artifactName" +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid CONTAINS $artifactName" +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching1(@Param("artifactName") String artifactName,
                                 Pageable page);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid CONTAINS  $artifactName AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid CONTAINS $artifactName AND artifact.storageId=$storageId AND artifact.repositoryId=$repositoryId " +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching2(@Param("artifactName") String artifactName,
                                 @Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 Pageable page);


}
