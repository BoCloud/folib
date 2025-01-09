package com.veadan.folib.model;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
//import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.github.zafarkhaja.semver.Version;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Generated;
import lombok.NoArgsConstructor;


/**
 * @author pj
 * @date 2021/7/26 16:03
 * @description  搜索结果
 */
//@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CargoSearchEntriesModel {
    private String name;

    @JsonProperty("max_version")
    private String maxVersion;

    private String description;

    @JsonIgnore
    private String path;


    public int compareVersion(String other) {
        return Version.valueOf(other).compareWithBuildsTo(Version.valueOf(this.maxVersion));
    }

    public int compareVersion(CargoSearchEntriesModel cargoSearchEntriesModel) {
        return Version.valueOf(this.maxVersion).compareWithBuildsTo(Version.valueOf(cargoSearchEntriesModel.maxVersion));
    }
}

