package com.veadan.folib.storage.search;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.dependency.snippet.CodeSnippet;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author mtodorov
 */
public class SearchResult
{

    @JsonProperty
    private ArtifactCoordinates artifactCoordinates;

    @JsonProperty
    private String storageId;

    @JsonProperty
    private String repositoryId;

    @JsonProperty
    private String url;

    /**
     * K: The compatible dependency format's alias
     * V: The string representation of the snippet.
     */
    @JsonProperty
    private List<CodeSnippet> snippets = new ArrayList<>();

    private Set<String> checksums = new HashSet<>();

    private Long sizeInBytes;

    public Long getSizeInBytes() {
        return sizeInBytes;
    }

    public void setSizeInBytes(Long sizeInBytes) {
        this.sizeInBytes = sizeInBytes;
    }

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

    public SearchResult()
    {
    }

    public SearchResult(String storageId,
                        String repositoryId,
                        ArtifactCoordinates artifactCoordinates,
                        String url)
    {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.artifactCoordinates = artifactCoordinates;
        this.url = url;
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

    public ArtifactCoordinates getArtifactCoordinates()
    {
        return artifactCoordinates;
    }

    public void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates)
    {
        this.artifactCoordinates = artifactCoordinates;
    }

    public String getPath()
    {
        return getArtifactCoordinates().buildPath();
    }

    public String getUrl()
    {
        return url;
    }

    public void setUrl(String url)
    {
        this.url = url;
    }

    public List<CodeSnippet> getSnippets()
    {
        return snippets;
    }

    public void setSnippets(List<CodeSnippet> snippets)
    {
        this.snippets = snippets;
    }

    @Override
    public String toString()
    {
        return getPath();
    }

}
