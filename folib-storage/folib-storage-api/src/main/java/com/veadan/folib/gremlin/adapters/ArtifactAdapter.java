package com.veadan.folib.gremlin.adapters;

import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.extractPropertyList;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.extractObject;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.toLocalDateTime;
import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.toLong;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.ArtifactArchiveListing;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import org.springframework.stereotype.Component;

/**
 * @author xuxinping
 */
@Component
public class ArtifactAdapter implements VertexEntityTraversalAdapter<Artifact>
{

    @Inject
    ArtifactCoordinatesHierarchyAdapter artifactCoordinatesAdapter;
    @Inject
    ArtifactTagAdapter artifactTagAdapter;

    @Override
    public String label()
    {
        return Vertices.ARTIFACT;
    }

    @Override
    public EntityTraversal<Vertex, Artifact> fold()
    {
        return fold(Optional.empty());
    }

    public EntityTraversal<Vertex, Artifact> fold(Optional<Class<? extends GenericArtifactCoordinates>> layoutArtifactCoordinatesClass)
    {
        return __.<Vertex, Object>project("id",
                                          "uuid",
                                          "storageId",
                                          "repositoryId",
                                          "lastUpdated",
                                          "lastUsed",
                                          "created",
                                          "sizeInBytes",
                                          "downloadCount",
                                          "filenames",
                                          "checksums",
                                          "artifactCoordinates",
                                          "tags",
                                          "artifactFileExists")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .by(__.enrichPropertyValue("storageId"))
                 .by(__.enrichPropertyValue("repositoryId"))
                 .by(__.enrichPropertyValue("lastUpdated"))
                 .by(__.enrichPropertyValue("lastUsed"))
                 .by(__.enrichPropertyValue("created"))
                 .by(__.enrichPropertyValue("sizeInBytes"))
                 .by(__.enrichPropertyValue("downloadCount"))
                 .by(__.enrichPropertyValues("filenames"))
                 .by(__.enrichPropertyValues("checksums"))
                 .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                       .mapToObject(__.inV()
                                      .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                      .map(EntityTraversalUtils::castToObject)))
                 .by(__.outE(Edges.ARTIFACT_HAS_TAGS)
                       .inV()
                       .map(artifactTagAdapter.fold())
                       .map(EntityTraversalUtils::castToObject)
                       .fold())
                 .by(__.enrichPropertyValue("artifactFileExists"))
                 .map(this::map);
    }

    private Artifact map(Traverser<Map<String, Object>> t)
    {
        String storageId = extractObject(String.class, t.get().get("storageId"));
        String repositoryId = extractObject(String.class, t.get().get("repositoryId"));
        ArtifactCoordinates artifactCoordinates = extractObject(ArtifactCoordinates.class,
                                                                t.get().get("artifactCoordinates"));

        ArtifactEntity result = new ArtifactEntity(storageId, repositoryId, artifactCoordinates);
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        result.setCreated(toLocalDateTime(extractObject(Long.class, t.get().get("created"))));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setLastUsed(toLocalDateTime(extractObject(Long.class, t.get().get("lastUsed"))));
        result.setSizeInBytes(extractObject(Long.class, t.get().get("sizeInBytes")));
        result.setDownloadCount(extractObject(Integer.class, t.get().get("downloadCount")));

        result.getArtifactArchiveListing()
              .setFilenames(extractPropertyList(String.class, t.get().get("filenames")).stream()
                                                                                       .filter(e -> !e.trim().isEmpty())
                                                                                       .collect(Collectors.toSet()));

        result.addChecksums(extractPropertyList(String.class, t.get().get("checksums")).stream()
                                                                                       .filter(e -> !e.trim().isEmpty())
                                                                                       .collect(Collectors.toSet()));

        List<ArtifactTag> tags = (List<ArtifactTag>) t.get().get("tags");
        result.setTagSet(new HashSet<>(tags));

        result.setArtifactFileExists(extractObject(Boolean.class, t.get().get("artifactFileExists")));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(Artifact entity)
    {
        ArtifactCoordinates artifactCoordinates = entity.getArtifactCoordinates();
        String storedArtifactId = Vertices.ARTIFACT + ":" + UUID.randomUUID().toString();

        Set<String> tagNames = entity.getTagSet().stream().map(ArtifactTag::getName).collect(Collectors.toSet());
        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.<Vertex, Edge>coalesce(__.<Vertex>outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES),
                                                                                    //cascading create ArtifactCoordinates only
                                                                                    createArtifactCoordinates(artifactCoordinates))
                                                            .outV()
                                                            .sideEffect(__.outE(Edges.ARTIFACT_HAS_TAGS).drop())
                                                            .map(unfoldArtifact(entity))
                                                            .store(storedArtifactId)
                                                            .sideEffect(__.V()
                                                                        .hasLabel(Vertices.ARTIFACT_TAG)
                                                                        .has("uuid", P.within(tagNames))
                                                                        .addE(Edges.ARTIFACT_HAS_TAGS)
                                                                        .from(__.select(storedArtifactId).unfold()));

        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT, entity, unfoldTraversal);
    }

    private Traversal<Vertex, Edge> createArtifactCoordinates(ArtifactCoordinates artifactCoordinates)
    {
        return __.<Vertex>addE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                 .to(saveArtifactCoordinates(artifactCoordinates));
    }

    private <S2> EntityTraversal<S2, Vertex> saveArtifactCoordinates(ArtifactCoordinates artifactCoordinates)
    {
        UnfoldEntityTraversal<Vertex, Vertex> artifactCoordinatesUnfold = artifactCoordinatesAdapter.unfold(artifactCoordinates);

        return __.<S2>V(artifactCoordinates)
                 .saveV(artifactCoordinates.getUuid(),
                        artifactCoordinatesUnfold);
    }

    private EntityTraversal<Vertex, Vertex> unfoldArtifact(Artifact entity)
    {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (entity.getStorageId() != null)
        {
            t = t.property(single, "storageId", entity.getStorageId());
        }
        if (entity.getRepositoryId() != null)
        {
            t = t.property(single, "repositoryId", entity.getRepositoryId());
        }
        if (entity.getCreated() != null)
        {
            t = t.property(single, "created", toLong(entity.getCreated()));
        }
        if (entity.getLastUpdated() != null)
        {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }
        if (entity.getLastUsed() != null)
        {
            t = t.property(single, "lastUsed", toLong(entity.getLastUsed()));
        }
        if (entity.getSizeInBytes() != null)
        {
            t = t.property(single, "sizeInBytes", entity.getSizeInBytes());
        }
        if (entity.getDownloadCount() != null)
        {
            t = t.property(single, "downloadCount", entity.getDownloadCount());
        }

        ArtifactArchiveListing artifactArchiveListing = entity.getArtifactArchiveListing();

        Set<String> filenames = artifactArchiveListing.getFilenames();
        t = t.sideEffect(__.properties("filenames").drop());
        t = t.property("filenames", filenames);

        Map<String, String> checksums = entity.getChecksums();
        Set<String> checkSumAlgo = new HashSet<>();
        for (String alg : checksums.keySet())
        {
            checkSumAlgo.add("{" + alg + "}" + checksums.get(alg));
        }
        t = t.sideEffect(__.properties("checksums").drop());
        t = t.property("checksums", checkSumAlgo);

        if (entity.getArtifactFileExists() != null)
        {
            t = t.property(single, "artifactFileExists", entity.getArtifactFileExists());
        }

        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>aggregate("x")
                 .select("x")
                 .unfold();
    }

}
