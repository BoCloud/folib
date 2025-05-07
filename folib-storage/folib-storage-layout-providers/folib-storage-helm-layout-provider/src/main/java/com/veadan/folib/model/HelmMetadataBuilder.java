package com.veadan.folib.model;




import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import java.util.Comparator;
import java.util.List;
import java.util.Map;

public class HelmMetadataBuilder {
    private final Map<String, HelmChartMetadata> chartCandidates = Maps.newHashMap();

    private List<HelmDependencyMetadata> dependencies = Lists.newArrayList();

    public void addChartCandidate(String path, HelmChartMetadata helmChartMetadata) {
        this.chartCandidates.put(path, helmChartMetadata);
    }

    public void setDependencies(List<HelmDependencyMetadata> dependencies) {
        this.dependencies = dependencies;
    }

    public HelmMetadata build() {
        HelmChartMetadata helmChart = selectMainChart();
        return new HelmMetadata(helmChart, calculateDependencies(helmChart, this.dependencies));
    }

    private HelmChartMetadata selectMainChart() {
        return this.chartCandidates.entrySet().stream()
                .min(Comparator.comparingInt(this::getDepth))
                .map(Map.Entry::getValue)
                .orElse(null);
    }

    private List<HelmDependencyMetadata> calculateDependencies(HelmChartMetadata metadata, List<HelmDependencyMetadata> dependencies) {
        if (metadata != null) {
            if ((metadata.dependencies == null || metadata.dependencies.isEmpty()) && dependencies != null &&
                    !dependencies.isEmpty()) {
                metadata.dependencies = dependencies;
            }
            if ("v2".equals(metadata.apiVersion)) {
                dependencies = metadata.dependencies;
            }
        }
        return dependencies;
    }

    private int getDepth(Map.Entry<String, HelmChartMetadata> entry) {
        int count = 0;
        for (byte b : ((String)entry.getKey()).getBytes()) {
            if (b == 47 || b == 92) {
                count++;
            }
        }
        return count;
    }
}
