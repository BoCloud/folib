package com.veadan.folib.domain;

/**
 * @author sbespalov
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
