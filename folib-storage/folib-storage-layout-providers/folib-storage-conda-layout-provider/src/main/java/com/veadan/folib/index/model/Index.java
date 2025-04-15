package com.veadan.folib.index.model;

import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.Generated;

import java.io.Serializable;
import java.util.List;


/**
 * @author LingengMa
 * @date 2025/04/09 15:11
 * @Description:
 */

@JsonNaming(PropertyNamingStrategy.SnakeCaseStrategy.class)
public class Index implements Serializable {
    private String arch;
    private String build;
    private Integer buildNumber;
    private List<String> constrains;
    private List<String> depends;
    private List<String> requires;
    private String license;
    private String licenseFamily;
    private String name;
    private String platform;
    private String subdir;
    private Long timestamp;
    private String version;
    private String features;
    private String trackFeatures;
    private Long mtime;
    private Object noarch;

    @Generated
    public String getArch() {
        return this.arch;
    }

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
    public List<String> getRequires() {
        return this.requires;
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
    public String getName() {
        return this.name;
    }

    @Generated
    public String getPlatform() {
        return this.platform;
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
    public Long getMtime() {
        return this.mtime;
    }

    @Generated
    public Object getNoarch() {
        return this.noarch;
    }

    @Generated
    public void setArch(final String arch) {
        this.arch = arch;
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
    public void setRequires(final List<String> requires) {
        this.requires = requires;
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
    public void setName(final String name) {
        this.name = name;
    }

    @Generated
    public void setPlatform(final String platform) {
        this.platform = platform;
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
    public void setMtime(final Long mtime) {
        this.mtime = mtime;
    }

    @Generated
    public void setNoarch(final Object noarch) {
        this.noarch = noarch;
    }

    @Generated
    public String toString() {
        String var10000 = this.getArch();
        return "Index(arch=" + var10000 + ", build=" + this.getBuild() + ", buildNumber=" + this.getBuildNumber() + ", constrains=" + this.getConstrains() + ", depends=" + this.getDepends() + ", requires=" + this.getRequires() + ", license=" + this.getLicense() + ", licenseFamily=" + this.getLicenseFamily() + ", name=" + this.getName() + ", platform=" + this.getPlatform() + ", subdir=" + this.getSubdir() + ", timestamp=" + this.getTimestamp() + ", version=" + this.getVersion() + ", features=" + this.getFeatures() + ", trackFeatures=" + this.getTrackFeatures() + ", mtime=" + this.getMtime() + ", noarch=" + this.getNoarch() + ")";
    }

    @Generated
    public Index() {
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
