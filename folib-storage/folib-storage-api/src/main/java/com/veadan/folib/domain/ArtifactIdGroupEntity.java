package com.veadan.folib.domain;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author veadan
 * @author veadan
 */
@NodeEntity(Vertices.ARTIFACT_ID_GROUP)
public class ArtifactIdGroupEntity extends DomainEntity implements ArtifactIdGroup
{

    private String storageId;

    private String repositoryId;

    private String name;

    private String metadata;

    @Relationship(type = Edges.ARTIFACT_GROUP_HAS_ARTIFACTS, direction = Relationship.OUTGOING)
    private Set<Artifact> artifacts = new HashSet<>();


    ArtifactIdGroupEntity()
    {
    }

    public ArtifactIdGroupEntity(String storageId,
                                 String repositoryId,
                                 String name)
    {
        setUuid(String.format("%s-%s-%s", storageId, repositoryId, name));
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.name = name;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    @Override
    public Set<Artifact> getArtifacts()
    {
        return Collections.unmodifiableSet(artifacts);
    }

    @Override
    public void addArtifact(Artifact artifact)
    {
        artifacts.remove(artifact);
        artifacts.add(artifact);
    }

    @Override
    public void setArtifacts(Set<Artifact> artifacts) {
        this.artifacts = artifacts;
    }

    @Override
    public void removeArtifact(Artifact artifact)
    {
        artifacts.remove(artifact);
    }

    @Override
    public String getMetadata() {
        return metadata;
    }

    @Override
    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }
}
