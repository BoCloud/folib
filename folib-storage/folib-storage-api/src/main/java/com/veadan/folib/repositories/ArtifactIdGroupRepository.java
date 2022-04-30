package com.veadan.folib.repositories;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.transaction.Transactional;

import com.veadan.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.veadan.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.gremlin.adapters.ArtifactIdGroupAdapter;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class ArtifactIdGroupRepository extends GremlinVertexRepository<ArtifactIdGroup>
{

    @Inject
    ArtifactIdGroupAdapter adapter;
    @Inject
    ArtifactIdGroupQueries queries;
    @Inject
    ConfigurationManager configurationManager;

    @Override
    protected ArtifactIdGroupAdapter adapter()
    {
        return adapter;
    }

    public Page<ArtifactIdGroup> findMatching(String storageId,
                                              String repositoryId,
                                              Pageable page)
    {
        return queries.findMatching(storageId, repositoryId, page);
    }

    public Optional<ArtifactIdGroup> findAllArtifactsInGroup(String storageId,
                                                             String repositoryId,
                                                             String artifactId)
    {
        return findArtifactsGroupWithTag(storageId, repositoryId, artifactId, Optional.empty());
    }

    public Optional<ArtifactIdGroup> findArtifactsGroupWithTag(String storageId,
                                                               String repositoryId,
                                                               String artifactId,
                                                               Optional<ArtifactTag> tag)
    {
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
        if (!t.hasNext())
        {
            return Optional.empty();
        }

        return Optional.of(t.next());
    }

    public Boolean artifactsExists(Set<String> storageRepositoryIds,
                                   String artifactId,
                                   Collection<String> coordinateValues)
    {
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
                               Collection<String> coordinateValues)
    {
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
                               Collection<String> coordinateValues)
    {

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
                                        Integer limit)
    {
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
                                        Integer limit)
    {
        return queries.findArtifacts(Collections.singleton(new ArtifactIdGroupEntity(storageId, repositoryId, artifactId))
                                                .stream()
                                                .map(ArtifactIdGroup::getUuid)
                                                .collect(Collectors.toSet()),
                                     coordinateValues,
                                     skip,
                                     limit);
    }
    
}

@Repository
interface ArtifactIdGroupQueries
        extends org.springframework.data.repository.Repository<ArtifactIdGroup, String>
{
    @Query(value = "MATCH (aig:`ArtifactIdGroup`) " +
                   "WHERE aig.storageId=$storageId and aig.repositoryId=$repositoryId " +
                   "WITH aig " +
                   "OPTIONAL MATCH (aig)-[r0:ArtifactGroupHasArtifacts]->(artifact:Artifact)-[r1]->(genericCoordinates:GenericArtifactCoordinates)<-[r2]-(layoutCoordinates) " +
                   "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates " +
                   "OPTIONAL MATCH (artifact)-[r4]->(tag:ArtifactTag) " +
                   "WITH aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag " +
                   "RETURN aig, r0, artifact, r1, genericCoordinates, r2, layoutCoordinates, r4, tag",
           countQuery = "MATCH (aig:`ArtifactIdGroup`) " +
                        "WHERE aig.storageId=$storageId and aig.repositoryId=$repositoryId " +
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
