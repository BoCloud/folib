package com.veadan.folib.domain;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.Transient;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.gremlin.adapters.DateConverter;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;

/**
 * @author Veadan
 * @author xuxinping
 */
@NodeEntity(Vertices.ARTIFACT)
public class ArtifactEntity
        extends DomainEntity implements Artifact
{

    private String storageId;

    private String repositoryId;

    @Relationship(type = Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES, direction = Relationship.OUTGOING)
    private GenericArtifactCoordinates artifactCoordinates;

    @Relationship(type = Edges.ARTIFACT_HAS_TAGS, direction = Relationship.OUTGOING)
    private Set<ArtifactTag> tagSet;

    private Set<String> checksums = new HashSet<>();

    private Set<String> filenames = new LinkedHashSet<>();

    private Long sizeInBytes;

    @Convert(DateConverter.class)
    private LocalDateTime lastUpdated;

    @Convert(DateConverter.class)
    private LocalDateTime lastUsed;

    @Convert(DateConverter.class)
    private LocalDateTime created;

    private Integer downloadCount = Integer.valueOf(0);

    private final ArtifactArchiveListing artifactArchiveListing = new ArtifactEntityArchiveListing();
    
    private Boolean artifactFileExists = Boolean.TRUE;
    
    ArtifactEntity()
    {
    }

    public ArtifactEntity(String storageId,
                          String repositoryId,
                          ArtifactCoordinates artifactCoordinates)
    {
        Objects.nonNull(artifactCoordinates);

        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.artifactCoordinates = artifactCoordinates;
        setUuid(String.format("%s-%s-%s", getStorageId(), getRepositoryId(), getArtifactCoordinates().buildPath()));
    }

    @Override
    public String getStorageId()
    {
        return storageId;
    }

    @Override
    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    @Override
    public String getRepositoryId()
    {
        return repositoryId;
    }

    @Override
    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    @Override
    public ArtifactCoordinates getArtifactCoordinates()
    {
        if (artifactCoordinates instanceof ArtifactCoordinates)
        {
            return (ArtifactCoordinates) artifactCoordinates;
        }
        
        return (ArtifactCoordinates) artifactCoordinates.getHierarchyChild();
    }

    @Override
    public void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates)
    {
        this.artifactCoordinates = artifactCoordinates;
    }

    @Override
    public Set<ArtifactTag> getTagSet()
    {
        return tagSet = Optional.ofNullable(tagSet).orElse(new HashSet<>());
    }

    public void setTagSet(Set<ArtifactTag> tagSet)
    {
        this.tagSet = tagSet;
    }

    @Override
    public Map<String, String> getChecksums()
    {
        return checksums.stream().filter(e -> !e.trim().isEmpty())
                                 .collect(Collectors.toMap(e -> e.substring(1, e.indexOf("}")),
                                                           e -> e.substring(e.indexOf("}") + 1)));
    }

    public void setChecksums(Map<String, String> checksums)
    {
        this.checksums.clear();
        this.checksums.addAll(checksums.entrySet()
                                       .stream()
                                       .map(e -> "{" + e.getKey() + "}" + e.getValue())
                                       .collect(Collectors.toSet()));
    }

    public void addChecksums(Set<String> checksums)
    {
        if (checksums == null)
        {
            return;
        }
        checksums.stream()
                 .filter(e -> e.startsWith("{"))
                 .filter(e -> e.indexOf("}") > 1)
                 .filter(e -> !e.endsWith("}"))
                 .forEach(this.checksums::add);
    }

    @Override
    public Long getSizeInBytes()
    {
        return sizeInBytes;
    }

    @Override
    public void setSizeInBytes(Long sizeInBytes)
    {
        this.sizeInBytes = sizeInBytes;
    }

    @Override
    public LocalDateTime getLastUpdated()
    {
        return lastUpdated;
    }

    @Override
    public void setLastUpdated(LocalDateTime lastUpdated)
    {
        this.lastUpdated = lastUpdated;
    }

    @Override
    public LocalDateTime getLastUsed()
    {
        return lastUsed;
    }

    @Override
    public void setLastUsed(LocalDateTime lastUsed)
    {
        this.lastUsed = lastUsed;
    }

    @Override
    public LocalDateTime getCreated()
    {
        return created;
    }

    @Override
    public void setCreated(LocalDateTime created)
    {
        this.created = created;
    }

    @Override
    public Integer getDownloadCount()
    {
        return downloadCount;
    }

    @Override
    public void setDownloadCount(Integer downloadCount)
    {
        this.downloadCount = downloadCount;
    }

    @Override
    public ArtifactArchiveListing getArtifactArchiveListing()
    {
        return artifactArchiveListing;
    }
    
    public Boolean getArtifactFileExists()
    {
        return artifactFileExists;
    }

    public void setArtifactFileExists(Boolean cached)
    {
        this.artifactFileExists = cached;
    }

    @Override
    @Transient
    public String getArtifactPath()
    {
        return Optional.of(getArtifactCoordinates())
                       .map(c -> c.buildPath())
                       .orElseThrow(() -> new IllegalStateException("ArtifactCoordinates required to be set."));
    }

    public class ArtifactEntityArchiveListing implements ArtifactArchiveListing
    {

        public Set<String> getFilenames()
        {
            return ArtifactEntity.this.filenames.stream().filter(e -> !e.isEmpty()).collect(Collectors.toSet());
        }

        public void setFilenames(final Set<String> filenames)
        {
            ArtifactEntity.this.filenames = filenames;
        }

    }

}
