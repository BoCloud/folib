package com.folib.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.util.List;
import java.util.Objects;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class HelmMetadata {
    public HelmChartMetadata helmChart;

    public List<HelmDependencyMetadata> dependencies;

    public HelmMetadata() {}

    public HelmMetadata(HelmChartMetadata helmChart, List<HelmDependencyMetadata> dependencies) {
        this.helmChart = helmChart;
        this.dependencies = dependencies;
    }

    public boolean equals(Object o) {
        HelmMetadata that;
        if (this == o)
            return true;
        if (o instanceof HelmMetadata) {
            that = (HelmMetadata)o;
        } else {
            return false;
        }
        return (Objects.equals(this.helmChart, that.helmChart) && Objects.equals(this.dependencies, that.dependencies));
    }

    public int hashCode() {
        int result = (this.helmChart != null) ? this.helmChart.hashCode() : 0;
        result = 31 * result + ((this.dependencies != null) ? this.dependencies.hashCode() : 0);
        return result;
    }
}
