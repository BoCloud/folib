package com.veadan.folib.storage.search;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.dependency.snippet.CodeSnippet;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author mtodorov
 */
@Data
@Builder
@AllArgsConstructor
public class SearchResult {

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


    private String lastUpdated;

    private String lastUsed;

    private String created;

    private String sha;

    private String md5;

    private List treeNode;

    /**
     * 制品名称
     */
    private String artifactName;
    /**
     * 制品path
     */
    private String artifactPath;
    /**
     * 仓库布局
     */
    private String layout;

    /**
     * 制品path
     */
    private String path;

    public List getTreeNode() {
        return treeNode;
    }

    public void setTreeNode(List treeNode) {
        this.treeNode = treeNode;
    }

    public String getLastUpdated() {
        return lastUpdated;
    }

    public void setLastUpdated(String lastUpdated) {
        this.lastUpdated = lastUpdated;
    }

    public String getLastUsed() {
        return lastUsed;
    }

    public void setLastUsed(String lastUsed) {
        this.lastUsed = lastUsed;
    }

    public String getCreated() {
        return created;
    }

    public void setCreated(String created) {
        this.created = created;
    }

    public String getSha() {
        return sha;
    }

    public void setSha(String sha) {
        this.sha = sha;
    }

    public String getMd5() {
        return md5;
    }

    public void setMd5(String md5) {
        this.md5 = md5;
    }

    public Integer getDownloadCount() {
        return downloadCount;
    }

    public void setDownloadCount(Integer downloadCount) {
        this.downloadCount = downloadCount;
    }

    private Integer downloadCount = Integer.valueOf(0);

    public Long getSizeInBytes() {
        return sizeInBytes;
    }

    public void setSizeInBytes(Long sizeInBytes) {
        this.sizeInBytes = sizeInBytes;
    }

    public Map<String, String> getChecksums() {
        return checksums.stream().filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toMap(e -> e.substring(1, e.indexOf("}")),
                        e -> e.substring(e.indexOf("}") + 1)));
    }

    public void setChecksums(Map<String, String> checksums) {
        this.checksums.clear();
        this.checksums.addAll(checksums.entrySet()
                .stream()
                .map(e -> "{" + e.getKey() + "}" + e.getValue())
                .collect(Collectors.toSet()));
    }

    public SearchResult() {
    }

    public SearchResult(String storageId,
                        String repositoryId,
                        ArtifactCoordinates artifactCoordinates,
                        String url) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.artifactCoordinates = artifactCoordinates;
        this.url = url;
    }

    public String getStorageId() {
        return storageId;
    }

    public void setStorageId(String storageId) {
        this.storageId = storageId;
    }

    public String getRepositoryId() {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId) {
        this.repositoryId = repositoryId;
    }

    public ArtifactCoordinates getArtifactCoordinates() {
        return artifactCoordinates;
    }

    public void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        this.artifactCoordinates = artifactCoordinates;
    }

    public String getPath() {
        return Objects.nonNull(artifactCoordinates) ? artifactCoordinates.buildPath() : "";
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public List<CodeSnippet> getSnippets() {
        return snippets;
    }

    public void setSnippets(List<CodeSnippet> snippets) {
        this.snippets = snippets;
    }

    @Override
    public String toString() {
        return getPath();
    }

}
