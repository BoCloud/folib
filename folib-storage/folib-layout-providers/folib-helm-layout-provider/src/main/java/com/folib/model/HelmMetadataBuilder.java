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
