package com.folib.domain;

import java.util.Set;

import com.folib.data.domain.DomainObject;

public interface ArtifactGroup extends DomainObject
{

    String getName();

    Set<Artifact> getArtifacts();

    void addArtifact(Artifact artifact);

    void setArtifacts(Set<Artifact> artifacts);

    void removeArtifact(Artifact artifact);

    String getMetadata();

    void setMetadata(String metadata);
}
