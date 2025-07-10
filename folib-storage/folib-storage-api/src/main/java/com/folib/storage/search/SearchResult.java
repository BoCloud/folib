/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.search;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.dependency.snippet.CodeSnippet;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
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
     * 仓库子布局
     */
    private String subLayout;
    /**
     * 制品path
     */
    private String path;

    /**
     * 获取docker 应用包下载地址
     */
    private Set<String> downloadFilesUrl;

    /**
     * metadata
     */
    private String metadata;

    /**
     * 漏洞数量
     */
    private Integer vulnerabilitiesCount;
    /**
     * 严重的漏洞数量
     */
    private Integer criticalVulnerabilitiesCount;
    /**
     * 高危的漏洞数量
     */
    private Integer highVulnerabilitiesCount;
    /**
     * 中危的漏洞数量
     */
    private Integer mediumVulnerabilitiesCount;
    /**
     * 低危的漏洞数量
     */
    private Integer lowVulnerabilitiesCount;
    /**
     * 被封存的漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount;

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

    private Integer downloadCount = 0;

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

    public String getMetadata() {
        return metadata;
    }

    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }

    @Override
    public String toString() {
        return getPath();
    }

}
