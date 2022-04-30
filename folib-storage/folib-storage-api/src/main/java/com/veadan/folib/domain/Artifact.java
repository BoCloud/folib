package com.veadan.folib.domain;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.Set;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.data.domain.DomainObject;

public interface Artifact extends DomainObject
{

    String getStorageId();

    void setStorageId(String storageId);

    String getRepositoryId();

    void setRepositoryId(String repositoryId);

    ArtifactCoordinates getArtifactCoordinates();

    void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates);

    Set<ArtifactTag> getTagSet();

    Map<String, String> getChecksums();

    void setChecksums(Map<String, String> digestMap);

    Long getSizeInBytes();

    void setSizeInBytes(Long sizeInBytes);

    LocalDateTime getLastUpdated();

    void setLastUpdated(LocalDateTime lastUpdated);

    LocalDateTime getLastUsed();

    void setLastUsed(LocalDateTime lastUsed);

    LocalDateTime getCreated();

    void setCreated(LocalDateTime created);

    Integer getDownloadCount();

    void setDownloadCount(Integer downloadCount);

    Boolean getArtifactFileExists();

    void setArtifactFileExists(Boolean cached);

    ArtifactArchiveListing getArtifactArchiveListing();

    String getArtifactPath();

}
