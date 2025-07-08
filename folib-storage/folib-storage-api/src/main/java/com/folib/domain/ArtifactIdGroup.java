package com.folib.domain;

/**
 * @author veadan
 */
public interface ArtifactIdGroup extends ArtifactGroup
{
    String getStorageId();

    String getRepositoryId();

    default String getArtifactId()
    {
        return getName();
    }
    
}
