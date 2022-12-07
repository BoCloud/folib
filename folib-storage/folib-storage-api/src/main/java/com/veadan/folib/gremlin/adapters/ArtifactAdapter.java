package com.veadan.folib.gremlin.adapters;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.*;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.dsl.__;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;

import static com.veadan.folib.gremlin.dsl.EntityTraversalUtils.*;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author xuxinping
 */
@Component
public class ArtifactAdapter implements VertexEntityTraversalAdapter<Artifact> {

    @Inject
    ArtifactCoordinatesHierarchyAdapter artifactCoordinatesAdapter;
    @Inject
    ArtifactTagAdapter artifactTagAdapter;
    @Inject
    VulnerabilityAdapter vulnerabilityAdapter;

    @Override
    public String label() {
        return Vertices.ARTIFACT;
    }

    @Override
    public EntityTraversal<Vertex, Artifact> fold() {
        return fold(Optional.empty());
    }

    public EntityTraversal<Vertex, Artifact> fold(Optional<Class<? extends GenericArtifactCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "evidenceQuantity",
                "vulnerabilities",
                "dependencyCount",
                "dependencyVulnerabilitiesCount",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "metadata",
                "filenames",
                "checksums",
                "artifactCoordinates",
                "tags",
                "vulnerabilitySet",
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
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValue("evidenceQuantity"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("dependencyCount"))
                .by(__.enrichPropertyValue("dependencyVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("metadata"))
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
                .by(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .inV()
                        .map(vulnerabilityAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .map(this::map);
    }

    public EntityTraversal<Vertex, VulnerabilityArtifactDomain> vulnerabilityFold() {
        return __.<Vertex, Object>project("id", "vulnerabilityID",
                "uuid",
                "storageId",
                "repositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "evidenceQuantity",
                "vulnerabilities",
                "dependencyCount",
                "dependencyVulnerabilitiesCount",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "filenames",
                "checksums",
                "artifactCoordinates",
                "tags",
                "vulnerabilitySet",
                "artifactFileExists")
                .by(__.id())
                .by(__.select("v").values("uuid"))
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValue("evidenceQuantity"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("dependencyCount"))
                .by(__.enrichPropertyValue("dependencyVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValues("filenames"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(Optional.empty()))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.outE(Edges.ARTIFACT_HAS_TAGS)
                        .inV()
                        .map(artifactTagAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .inV()
                        .map(vulnerabilityAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .map(this::vulnerabilityMap);
    }

    private VulnerabilityArtifactDomain vulnerabilityMap(Traverser<Map<String, Object>> t) {
        String storageId = extractObject(String.class, t.get().get("storageId"));
        String repositoryId = extractObject(String.class, t.get().get("repositoryId"));
        ArtifactCoordinates artifactCoordinates = extractObject(ArtifactCoordinates.class,
                t.get().get("artifactCoordinates"));

        VulnerabilityArtifactDomain result = new VulnerabilityArtifactDomain();
        result.setStorageId(storageId);
        result.setRepositoryId(repositoryId);
        result.setArtifactCoordinates(artifactCoordinates);
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setVulnerabilityId(extractObject(String.class, t.get().get("vulnerabilityID")));
        result.setCreated(toLocalDateTime(extractObject(Long.class, t.get().get("created"))));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setLastUsed(toLocalDateTime(extractObject(Long.class, t.get().get("lastUsed"))));
        result.setSizeInBytes(extractObject(Long.class, t.get().get("sizeInBytes")));
        result.setDownloadCount(extractObject(Integer.class, t.get().get("downloadCount")));

        result.setSafeLevel(extractObject(String.class, t.get().get("safeLevel")));
        result.setEvidenceQuantity(extractObject(Integer.class, t.get().get("evidenceQuantity")));
        result.setVulnerabilities(extractPropertyList(String.class, t.get().get("vulnerabilities")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setDependencyCount(extractObject(Integer.class, t.get().get("dependencyCount")));
        result.setDependencyVulnerabilitiesCount(extractObject(Integer.class, t.get().get("dependencyVulnerabilitiesCount")));
        result.setVulnerabilitiesCount(extractObject(Integer.class, t.get().get("vulnerabilitiesCount")));
        result.setCriticalVulnerabilitiesCount(extractObject(Integer.class, t.get().get("criticalVulnerabilitiesCount")));
        result.setHighVulnerabilitiesCount(extractObject(Integer.class, t.get().get("highVulnerabilitiesCount")));
        result.setMediumVulnerabilitiesCount(extractObject(Integer.class, t.get().get("mediumVulnerabilitiesCount")));
        result.setLowVulnerabilitiesCount(extractObject(Integer.class, t.get().get("lowVulnerabilitiesCount")));
        result.setSuppressedVulnerabilitiesCount(extractObject(Integer.class, t.get().get("suppressedVulnerabilitiesCount")));

        result.getArtifactArchiveListing()
                .setFilenames(extractPropertyList(String.class, t.get().get("filenames")).stream()
                        .filter(e -> !e.trim().isEmpty())
                        .collect(Collectors.toSet()));

        result.addChecksums(extractPropertyList(String.class, t.get().get("checksums")).stream()
                .filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toSet()));

        List<ArtifactTag> tags = (List<ArtifactTag>) t.get().get("tags");
        result.setTagSet(new HashSet<>(tags));

        Object vulnerabilityObject = t.get().get("vulnerabilitySet");
        if (Objects.nonNull(vulnerabilityObject)) {
            List<Vulnerability> vulnerabilityList = (List<Vulnerability>) vulnerabilityObject;
            result.setVulnerabilitySet(new LinkedHashSet<>(vulnerabilityList));
        }

        result.setArtifactFileExists(extractObject(Boolean.class, t.get().get("artifactFileExists")));

        return result;
    }

    private Artifact map(Traverser<Map<String, Object>> t) {
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

        result.setSafeLevel(extractObject(String.class, t.get().get("safeLevel")));
        result.setEvidenceQuantity(extractObject(Integer.class, t.get().get("evidenceQuantity")));
        result.setVulnerabilities(extractPropertyList(String.class, t.get().get("vulnerabilities")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setDependencyCount(extractObject(Integer.class, t.get().get("dependencyCount")));
        result.setDependencyVulnerabilitiesCount(extractObject(Integer.class, t.get().get("dependencyVulnerabilitiesCount")));
        result.setVulnerabilitiesCount(extractObject(Integer.class, t.get().get("vulnerabilitiesCount")));
        result.setCriticalVulnerabilitiesCount(extractObject(Integer.class, t.get().get("criticalVulnerabilitiesCount")));
        result.setHighVulnerabilitiesCount(extractObject(Integer.class, t.get().get("highVulnerabilitiesCount")));
        result.setMediumVulnerabilitiesCount(extractObject(Integer.class, t.get().get("mediumVulnerabilitiesCount")));
        result.setLowVulnerabilitiesCount(extractObject(Integer.class, t.get().get("lowVulnerabilitiesCount")));
        result.setSuppressedVulnerabilitiesCount(extractObject(Integer.class, t.get().get("suppressedVulnerabilitiesCount")));


        result.getArtifactArchiveListing()
                .setFilenames(extractPropertyList(String.class, t.get().get("filenames")).stream()
                        .filter(e -> !e.trim().isEmpty())
                        .collect(Collectors.toSet()));

        result.addChecksums(extractPropertyList(String.class, t.get().get("checksums")).stream()
                .filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toSet()));

        List<ArtifactTag> tags = (List<ArtifactTag>) t.get().get("tags");
        result.setTagSet(new HashSet<>(tags));

        Object vulnerabilityObject = t.get().get("vulnerabilitySet");
        if (Objects.nonNull(vulnerabilityObject)) {
            List<Vulnerability> vulnerabilityList = (List<Vulnerability>) vulnerabilityObject;
            result.setVulnerabilitySet(new LinkedHashSet<>(vulnerabilityList));
        }

        result.setArtifactFileExists(extractObject(Boolean.class, t.get().get("artifactFileExists")));
        result.setMetadata(extractObject(String.class, t.get().get("metadata")));
        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(Artifact entity) {
        ArtifactCoordinates artifactCoordinates = entity.getArtifactCoordinates();
        String storedArtifactId = Vertices.ARTIFACT + ":" + UUID.randomUUID().toString();

        Set<String> tagNames = entity.getTagSet().stream().map(ArtifactTag::getName).collect(Collectors.toSet());
        Set<String> vulnerabilities = entity.getVulnerabilities();
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
                        .from(__.select(storedArtifactId).unfold()))
                .sideEffect(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES).drop());
        if (CollectionUtils.isNotEmpty(vulnerabilities)) {
            for (String vulnerability : vulnerabilities) {
                EntityTraversal<Object, Vertex> vulnerabilityEntityTraversal = __.V()
                        .hasLabel(Vertices.VULNERABILITY)
                        .has("uuid", vulnerability);
                vulnerabilityEntityTraversal = vulnerabilityEntityTraversal.addE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .from(__.<Vertex, Vertex>select(storedArtifactId).unfold())
                        .property(Properties.VULNERABILITY_ID, vulnerability).inV();
                unfoldTraversal = unfoldTraversal.sideEffect(vulnerabilityEntityTraversal);
            }
        }
        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT, entity, unfoldTraversal);
    }

    private Traversal<Vertex, Edge> createArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        return __.<Vertex>addE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                .to(saveArtifactCoordinates(artifactCoordinates));
    }

    private <S2> EntityTraversal<S2, Vertex> saveArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        UnfoldEntityTraversal<Vertex, Vertex> artifactCoordinatesUnfold = artifactCoordinatesAdapter.unfold(artifactCoordinates);

        return __.<S2>V(artifactCoordinates)
                .saveV(artifactCoordinates.getUuid(),
                        artifactCoordinatesUnfold);
    }

    private EntityTraversal<Vertex, Vertex> unfoldArtifact(Artifact entity) {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (entity.getStorageId() != null) {
            t = t.property(single, "storageId", entity.getStorageId());
        }
        if (entity.getRepositoryId() != null) {
            t = t.property(single, "repositoryId", entity.getRepositoryId());
        }
        if (entity.getCreated() != null) {
            t = t.property(single, "created", toLong(entity.getCreated()));
        }
        if (entity.getLastUpdated() != null) {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }
        if (entity.getLastUsed() != null) {
            t = t.property(single, "lastUsed", toLong(entity.getLastUsed()));
        }
        if (entity.getSizeInBytes() != null) {
            t = t.property(single, "sizeInBytes", entity.getSizeInBytes());
        }
        if (entity.getDownloadCount() != null) {
            t = t.property(single, "downloadCount", entity.getDownloadCount());
        }
        if (entity.getSafeLevel() != null) {
            t = t.property(single, "safeLevel", entity.getSafeLevel());
        }
        if (entity.getDependencyCount() != null) {
            t = t.property(single, "dependencyCount", entity.getDependencyCount());
        }
        if (entity.getDependencyVulnerabilitiesCount() != null) {
            t = t.property(single, "dependencyVulnerabilitiesCount", entity.getDependencyVulnerabilitiesCount());
        }
        if (entity.getVulnerabilitiesCount() != null) {
            t = t.property(single, "vulnerabilitiesCount", entity.getVulnerabilitiesCount());
        }
        if (entity.getCriticalVulnerabilitiesCount() != null) {
            t = t.property(single, "criticalVulnerabilitiesCount", entity.getCriticalVulnerabilitiesCount());
        }
        if (entity.getHighVulnerabilitiesCount() != null) {
            t = t.property(single, "highVulnerabilitiesCount", entity.getHighVulnerabilitiesCount());
        }
        if (entity.getMediumVulnerabilitiesCount() != null) {
            t = t.property(single, "mediumVulnerabilitiesCount", entity.getMediumVulnerabilitiesCount());
        }
        if (entity.getLowVulnerabilitiesCount() != null) {
            t = t.property(single, "lowVulnerabilitiesCount", entity.getLowVulnerabilitiesCount());
        }
        if (entity.getSuppressedVulnerabilitiesCount() != null) {
            t = t.property(single, "suppressedVulnerabilitiesCount", entity.getSuppressedVulnerabilitiesCount());
        }
        if (entity.getEvidenceQuantity() != null) {
            t = t.property(single, "evidenceQuantity", entity.getEvidenceQuantity());
        }
        if (entity.getVulnerabilities() != null) {
            t = t.sideEffect(__.properties("vulnerabilities").drop());
            t = t.property("vulnerabilities", entity.getVulnerabilities());
        }
        if (StringUtils.isNotBlank(entity.getMetadata())) {
            t = t.property(single, "metadata", entity.getMetadata());
        }
        ArtifactArchiveListing artifactArchiveListing = entity.getArtifactArchiveListing();

        Set<String> filenames = artifactArchiveListing.getFilenames();
        t = t.sideEffect(__.properties("filenames").drop());
        t = t.property("filenames", filenames);

        Map<String, String> checksums = entity.getChecksums();
        Set<String> checkSumAlgo = new HashSet<>();
        for (String alg : checksums.keySet()) {
            checkSumAlgo.add("{" + alg + "}" + checksums.get(alg));
        }
        t = t.sideEffect(__.properties("checksums").drop());
        t = t.property("checksums", checkSumAlgo);

        if (entity.getArtifactFileExists() != null) {
            t = t.property(single, "artifactFileExists", entity.getArtifactFileExists());
        }

        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade() {
        return __.<Vertex>aggregate("x")
                .select("x")
                .unfold();
    }

}
