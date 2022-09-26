package com.veadan.folib.repositories;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import javax.inject.Inject;
import javax.transaction.Transactional;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.__;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.groovy.jsr223.GroovyTranslator;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.Scope;
import org.janusgraph.core.attribute.Text;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;


@Repository
@Transactional
public class ArtifactRepository extends GremlinVertexRepository<Artifact>
{
//查找标记
    @Inject
    ArtifactAdapter artifactAdapter;
    @Inject
    ArtifactEntityQueries queries;
    @Inject
    ConfigurationManager configurationManager;

    @Override
    protected ArtifactAdapter adapter()
    {
        return artifactAdapter;
    }

    public List<Artifact> findByPathLike(String storageId,
                                         String repositoryId,
                                         String path)
    {
        return EntityTraversalUtils.reduceHierarchy(queries.findByPathLike(storageId, repositoryId, path));
    }

    public Page<Artifact> findMatching(Integer lastAccessedTimeInDays,
                                       Long minSizeInBytes,
                                       Pageable pagination)
    {
        LocalDateTime date = Optional.ofNullable(lastAccessedTimeInDays)
                                     .map(v -> LocalDateTime.now().minusDays(lastAccessedTimeInDays))
                                     .orElse(null);
        return findMatching(date, minSizeInBytes, pagination);
    }

    public Page<Artifact> findMatching(LocalDateTime lastAccessedDate,
                                       Long minSizeInBytes,
                                       Pageable pagination)
    {
        Page<Artifact> result = queries.findMatching(lastAccessedDate, minSizeInBytes, pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching1(String artifactName,
                                       Pageable pagination)
    {
        Page<Artifact> result = queries.findMatching1(artifactName,pagination);

        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }

    public Page<Artifact> findMatching2(String artifactName,
                                        String storageId,
                                        String repositoryId,
                                        Pageable pagination)
    {
        Page<Artifact> result = queries.findMatching2(artifactName,storageId,repositoryId,pagination);
        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(result.toList()), pagination, result.getTotalElements());
    }
    public Boolean artifactEntityExists(String storageId,
                                        String repositoryId,
                                        String path)
    {
        return Optional.ofNullable(queries.artifactEntityExists(storageId, repositoryId, path)).orElse(Boolean.FALSE);
    }

    public Page<Artifact> findMatchingByIndex(Pageable pagination, String artifactName,
                                     String storageId,
                                     String repositoryId)
    {
        Long count = buildEntityTraversal(artifactName, storageId, repositoryId).count().tryNext().orElse(0L);
        long low =  pagination.getPageNumber() * pagination.getPageSize();
        long high =  (pagination.getPageNumber() + 1) * pagination.getPageSize();
        com.veadan.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        List<Artifact> artifactList = buildEntityTraversal(artifactName, storageId, repositoryId)
                .range(low,high)
                .map(artifactAdapter.fold(Optional.ofNullable(repository)
                        .map(com.veadan.folib.storage.repository.Repository::getLayout)
                        .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                        .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).toList();
        return new PageImpl<>(EntityTraversalUtils.reduceHierarchy(artifactList), pagination, count);
    }

    private EntityTraversal<Vertex, Vertex> buildEntityTraversal(String artifactName,
                                                                 String storageId,
                                                                 String repositoryId){
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT)
                .has("uuid", Text.textContains(artifactName));
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
                                  String path)
    {
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
                                    String path)
    {

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
        if (!t.hasNext())
        {
            return null;
        }
        return t.next();
    }

}

@Repository
interface ArtifactEntityQueries extends org.springframework.data.repository.Repository<Artifact, String>
{

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
           "WHERE genericCoordinates.uuid STARTS WITH $path and artifact.storageId=$storageId and artifact.repositoryId=$repositoryId " +
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
                   "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) and artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
                   "WITH artifact, r1, genericCoordinates " +
                   "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
                   "WITH artifact, r1, genericCoordinates, r4, tag " +
                   "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
                   "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
                   "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
           countQuery = "MATCH (artifact:Artifact) " +
                        "WHERE artifact.lastUsed <= coalesce($lastAccessedDate, artifact.lastUsed) and artifact.sizeInBytes >=  coalesce($minSizeInBytes, artifact.sizeInBytes) " +
                        "RETURN count(artifact)")
    Page<Artifact> findMatching(@Param("lastAccessedDate") LocalDateTime lastAccessedDate,
                                @Param("minSizeInBytes") Long minSizeInBytes,
                                Pageable page);

    @Query("MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
           "WHERE genericCoordinates.uuid=$path and artifact.storageId=$storageId and artifact.repositoryId=$repositoryId " +
           "RETURN EXISTS(artifact.uuid)")
    Boolean artifactEntityExists(@Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 @Param("path") String path);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid Contains  $artifactName" +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid Contains $artifactName" +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching1(@Param("artifactName") String artifactName,
                                Pageable page);

    @Query(value = "MATCH (genericCoordinates:GenericArtifactCoordinates)<-[r1]-(artifact:Artifact) " +
            "WHERE artifact.uuid Contains  $artifactName and artifact.storageId=$storageId and artifact.repositoryId=$repositoryId " +
            "WITH artifact, r1, genericCoordinates " +
            "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
            "WITH artifact, r1, genericCoordinates, r4, tag " +
            "MATCH (genericCoordinates)<-[r2]-(layoutCoordinates) " +
            "WITH artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
            "RETURN artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
            countQuery = "MATCH (artifact:Artifact) " +
                    "WHERE  artifact.uuid CONTAINS $artifactName and artifact.storageId=$storageId and artifact.repositoryId=$repositoryId " +
                    "RETURN count(artifact)")
    Page<Artifact> findMatching2(@Param("artifactName") String artifactName,
                                 @Param("storageId") String storageId,
                                 @Param("repositoryId") String repositoryId,
                                 Pageable page);



}
