package com.veadan.folib.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.veadan.folib.model.seaializer.ChartAnnotationsSerializer;
import com.veadan.folib.util.HelmVersionUtil;
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
