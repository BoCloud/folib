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
package com.folib.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.folib.model.seaializer.ChartAnnotationsSerializer;
import com.folib.util.HelmVersionUtil;
import com.github.zafarkhaja.semver.Version;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Map;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class HelmChartMetadata implements Comparable<HelmChartMetadata> {
    public String apiVersion;

    @JsonProperty("appVersion")
    public String appVersion;

    public String created;

    public Boolean deprecated;

    public String description;

    public String digest;

    public String engine;

    public String home;

    public String icon;

    public List<String> keywords;

    public List<HelmMaintainerModel> maintainers;

    public String name;

    public List<String> sources;

    public List<String> urls;

    @JsonProperty("version")
    public String version;

    public String type;

    public String kubeVersion;

    public List<HelmDependencyMetadata> dependencies;

    public Map<String, JsonNode> annotations;

    @JsonProperty("version")
    public String getMarkedVersion() {
        return HelmVersionUtil.markWithReplacePattern(this.version);
    }

    @JsonProperty("appVersion")
    public String getMarkedAppVersion() {
        return HelmVersionUtil.markWithReplacePattern(this.appVersion);
    }

    @JsonSerialize(using = ChartAnnotationsSerializer.class)
    public Map<String, JsonNode> getAnnotations() {
        return this.annotations;
    }

    @Override
    public int compareTo(@Nonnull HelmChartMetadata other) {
        int i = this.name.compareTo(other.name);
        if (i != 0) {
            return i;
        }
        try {
            return Version.valueOf(other.version).compareWithBuildsTo(Version.valueOf(this.version));
        } catch (Exception e) {
            return other.version.compareTo(this.version);
        }
    }
}
