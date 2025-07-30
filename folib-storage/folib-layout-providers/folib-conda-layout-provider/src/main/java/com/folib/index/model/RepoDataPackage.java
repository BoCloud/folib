package com.folib.index.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.Generated;

import java.util.List;


@JsonIgnoreProperties(ignoreUnknown = true)
@JsonPropertyOrder(
        alphabetic = true
)
@JsonNaming(PropertyNamingStrategy.SnakeCaseStrategy.class)
public class RepoDataPackage {
    private String build;
    private Integer buildNumber;
    private List<String> constrains;
    private List<String> depends;
    private String license;
    private String licenseFamily;
    private String md5;
    private String name;
    private Object noarch;
    private String sha256;
    private Long size;
    private String subdir;
    private Long timestamp;
    private String version;
    private String features;
    private String trackFeatures;
    @JsonInclude(Include.NON_DEFAULT)
    private boolean revoked;
    @JsonIgnore
    private Index index;
    @JsonIgnore
    private CondaVersion condaVersion;
    @JsonIgnore
    private String fileName;
    @JsonIgnore
    private boolean condaFile;

    @Generated
    public String getBuild() {
        return this.build;
    }

    @Generated
    public Integer getBuildNumber() {
        return this.buildNumber;
    }

    @Generated
    public List<String> getConstrains() {
        return this.constrains;
    }

    @Generated
    public List<String> getDepends() {
        return this.depends;
    }

    @Generated
    public String getLicense() {
        return this.license;
    }

    @Generated
    public String getLicenseFamily() {
        return this.licenseFamily;
    }

    @Generated
    public String getMd5() {
        return this.md5;
    }

    @Generated
    public String getName() {
        return this.name;
    }

    @Generated
    public Object getNoarch() {
        return this.noarch;
    }

    @Generated
    public String getSha256() {
        return this.sha256;
    }

    @Generated
    public Long getSize() {
        return this.size;
    }

    @Generated
    public String getSubdir() {
        return this.subdir;
    }

    @Generated
    public Long getTimestamp() {
        return this.timestamp;
    }

    @Generated
    public String getVersion() {
        return this.version;
    }

    @Generated
    public String getFeatures() {
        return this.features;
    }

    @Generated
    public String getTrackFeatures() {
        return this.trackFeatures;
    }

    @Generated
    public boolean isRevoked() {
        return this.revoked;
    }

    @Generated
    public Index getIndex() {
        return this.index;
    }

    @Generated
    public CondaVersion getCondaVersion() {
        return this.condaVersion;
    }

    @Generated
    public String getFileName() {
        return this.fileName;
    }

    @Generated
    public boolean isCondaFile() {
        return this.condaFile;
    }

    @Generated
    public void setBuild(final String build) {
        this.build = build;
    }

    @Generated
    public void setBuildNumber(final Integer buildNumber) {
        this.buildNumber = buildNumber;
    }

    @Generated
    public void setConstrains(final List<String> constrains) {
        this.constrains = constrains;
    }

    @Generated
    public void setDepends(final List<String> depends) {
        this.depends = depends;
    }

    @Generated
    public void setLicense(final String license) {
        this.license = license;
    }

    @Generated
    public void setLicenseFamily(final String licenseFamily) {
        this.licenseFamily = licenseFamily;
    }

    @Generated
    public void setMd5(final String md5) {
        this.md5 = md5;
    }

    @Generated
    public void setName(final String name) {
        this.name = name;
    }

    @Generated
    public void setNoarch(final Object noarch) {
        this.noarch = noarch;
    }

    @Generated
    public void setSha256(final String sha256) {
        this.sha256 = sha256;
    }

    @Generated
    public void setSize(final Long size) {
        this.size = size;
    }

    @Generated
    public void setSubdir(final String subdir) {
        this.subdir = subdir;
    }

    @Generated
    public void setTimestamp(final Long timestamp) {
        this.timestamp = timestamp;
    }

    @Generated
    public void setVersion(final String version) {
        this.version = version;
    }

    @Generated
    public void setFeatures(final String features) {
        this.features = features;
    }

    @Generated
    public void setTrackFeatures(final String trackFeatures) {
        this.trackFeatures = trackFeatures;
    }

    @Generated
    public void setRevoked(final boolean revoked) {
        this.revoked = revoked;
    }

    @JsonIgnore
    @Generated
    public void setIndex(final Index index) {
        this.index = index;
    }

    @JsonIgnore
    @Generated
    public void setCondaVersion(final CondaVersion condaVersion) {
        this.condaVersion = condaVersion;
    }

    @JsonIgnore
    @Generated
    public void setFileName(final String fileName) {
        this.fileName = fileName;
    }

    @JsonIgnore
    @Generated
    public void setCondaFile(final boolean condaFile) {
        this.condaFile = condaFile;
    }

    @Generated
    public String toString() {
        String var10000 = this.getBuild();
        return "RepoDataPackage(build=" + var10000 + ", buildNumber=" + this.getBuildNumber() + ", constrains=" + this.getConstrains() + ", depends=" + this.getDepends() + ", license=" + this.getLicense() + ", licenseFamily=" + this.getLicenseFamily() + ", md5=" + this.getMd5() + ", name=" + this.getName() + ", noarch=" + this.getNoarch() + ", sha256=" + this.getSha256() + ", size=" + this.getSize() + ", subdir=" + this.getSubdir() + ", timestamp=" + this.getTimestamp() + ", version=" + this.getVersion() + ", features=" + this.getFeatures() + ", trackFeatures=" + this.getTrackFeatures() + ", revoked=" + this.isRevoked() + ", index=" + this.getIndex() + ",  fileName=" + this.getFileName() + ", condaFile=" + this.isCondaFile() + ")";
    }

    @Generated
    public RepoDataPackage() {
    }


    @Generated
    public String toJson() {
        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            mapper.setSerializationInclusion(Include.NON_NULL);
            return mapper.writeValueAsString(this);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            throw new RuntimeException("Failed to convert Index to JSON", e);
        }
    }

    @Generated
    public String toJsonPretty() {
        try {
            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            mapper.setSerializationInclusion(Include.NON_NULL);
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(this);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            throw new RuntimeException("Failed to convert Index to JSON", e);
        }
    }

}
