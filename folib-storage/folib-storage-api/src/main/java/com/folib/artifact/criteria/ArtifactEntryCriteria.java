package com.folib.artifact.criteria;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 *
 */
public class ArtifactEntryCriteria
{

    private String storageId;
    private String repositoryId;
    private Map<String, String> coordinates = new HashMap<>();
    private Set<String> tagSet = new HashSet<>();

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

    public Map<String, String> getCoordinates()
    {
        return coordinates;
    }

    public void setCoordinates(Map<String, String> coordinates)
    {
        this.coordinates = coordinates;
    }

    public Set<String> getTagSet()
    {
        return tagSet;
    }

    public void setTagSet(Set<String> tagSet)
    {
        this.tagSet = tagSet;
    }

}
