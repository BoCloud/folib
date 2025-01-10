package com.veadan.folib.model;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.github.zafarkhaja.semver.Version;
import com.google.common.collect.Maps;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import lombok.*;
import org.apache.commons.compress.utils.Lists;
import software.amazon.awssdk.utils.CollectionUtils;

/**
 * @author veadan
 * @date 2021/12/27 15:05
 * @description cargo 元数据
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CargoMetadata implements Comparable<CargoMetadata> {
    private String name;

    private String vers;

    private String description;

    private String documentation;

    private String homepage;

    private String readmeFile;

    private String links;

    private List<String> authors;

    private List<String> keywords;

    private List<String> categories;

    private String license;

    private String licenseFile;

    private String repository;

    private Map<String, Map<String, String>> badges;

    @JsonUnwrapped
    private CargoLongMetadata cargoLongMetadata = new CargoLongMetadata();


    public Map<String, List<String>> getFeatures() {
        return this.cargoLongMetadata.getFeatures();
    }

    public List<CargoDependencyMetadata> getDeps() {
        return this.cargoLongMetadata.getDeps();
    }

    public void setFeatures(Map<String, List<String>> features) {
        this.cargoLongMetadata.setFeatures(features);
    }

    public void setDeps(List<CargoDependencyMetadata> deps) {
        this.cargoLongMetadata.setDeps(deps);
    }

    public boolean getYanked(boolean yanked) {
        return this.cargoLongMetadata.getYanked();
    }

    public void setYanked(boolean yanked) {
        this.cargoLongMetadata.setYanked(yanked);
    }


    public int compareTo(CargoMetadata other) {
        if (other == null)
            throw new NullPointerException("other is marked non-null but is null");
        int res = this.name.compareTo(other.getName());
        if (res != 0)
            return res;
        return Version.valueOf(other.vers).compareWithBuildsTo(Version.valueOf(this.vers));
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    public static class CargoLongMetadata implements Serializable {

        private List<CargoDependencyMetadata> deps = Lists.newArrayList();
        private Map<String, List<String>> features = Maps.newHashMap();

        @Setter
        private boolean yanked;

        @JsonIgnore
        public boolean isEmpty() {
            return (CollectionUtils.isNullOrEmpty(this.deps) && this.features.isEmpty());
        }


        public boolean getYanked() {
            return this.yanked;
        }
    }
}

