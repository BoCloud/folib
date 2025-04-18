package com.veadan.folib.index.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.Generated;

import java.util.HashSet;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;


/**
 * @author LingengMa
 * @date 2025/04/09 14:59
 * @Description:
 */

@JsonPropertyOrder({"info", "packages", "packages.conda", "removed"})
public class RepoData {
    private RepoDataInfo info = new RepoDataInfo("");
    private SortedMap<String, RepoDataPackage> packages = new TreeMap();
    @JsonProperty("packages.conda")
    private SortedMap<String, RepoDataPackage> condaPackages = new TreeMap();
    private Set<String> removed = new HashSet();

    @Generated
    public RepoDataInfo getInfo() {
        return this.info;
    }

    @Generated
    public SortedMap<String, RepoDataPackage> getPackages() {
        return this.packages;
    }

    @Generated
    public SortedMap<String, RepoDataPackage> getCondaPackages() {
        return this.condaPackages;
    }

    @Generated
    public Set<String> getRemoved() {
        return this.removed;
    }

    @Generated
    public void setInfo(final RepoDataInfo info) {
        this.info = info;
    }

    @Generated
    public void setPackages(final SortedMap<String, RepoDataPackage> packages) {
        this.packages = packages;
    }

    @JsonProperty("packages.conda")
    @Generated
    public void setCondaPackages(final SortedMap<String, RepoDataPackage> condaPackages) {
        this.condaPackages = condaPackages;
    }

    @Generated
    public void setRemoved(final Set<String> removed) {
        this.removed = removed;
    }


    @Generated
    public String toString() {
        RepoDataInfo var10000 = this.getInfo();
        return "RepoData(info=" + var10000 + ", packages=" + this.getPackages() + ", condaPackages=" + this.getCondaPackages() + ", removed=" + this.getRemoved() + ")";
    }

    @Generated
    public RepoData() {
    }

    @Generated
    public void update(RepoData repoData) {
        this.info = repoData.getInfo();
        this.packages = repoData.getPackages();
        this.condaPackages = repoData.getCondaPackages();
        this.removed = repoData.getRemoved();
    }


    @Generated
    public String toJson() {
        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            mapper.setSerializationInclusion(com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL);
            return mapper.writeValueAsString(this);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            throw new RuntimeException("Failed to convert Index to JSON", e);
        }
    }

    @Generated
    public String toJsonPretty() {
        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            mapper.setSerializationInclusion(com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL);
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(this);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            throw new RuntimeException("Failed to convert Index to JSON", e);
        }
    }
}
