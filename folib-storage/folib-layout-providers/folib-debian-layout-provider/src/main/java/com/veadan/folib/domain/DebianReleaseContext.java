package com.veadan.folib.domain;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.constant.DebianConstant;

import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * @author huayanjun
 * @since 2024-09-04 16:37
 */
public class DebianReleaseContext {
    private final String distribution;
    private final List<String> components;
    private final List<String> architectures;
    private final String releasePath;


    public DebianReleaseContext(String distribution, Set<String> components, Set<String> architectures) {
        this.distribution = distribution;
        this.components = sortedUniqueValues(components);
        this.architectures = sortedUniqueValues(architectures);
        this.releasePath = this.calcReleasePath();
    }

    public DebianReleaseContext(String distribution) {
        this.distribution = distribution;
        this.components = null;
        this.architectures = null;
        this.releasePath = this.calcReleasePath();
    }

    public String getDistribution() {
        return this.distribution;
    }

    public String[] getComponents() {
        return this.components.toArray(new String[0]);
    }

    public String[] getArchitectures() {
        return this.architectures.toArray(new String[0]);
    }

    public String getReleasePath() {
        return this.releasePath;
    }

    private String calcReleasePath() {
        return String.format("%s/%s", DebianConstant.PACKAGE_PREFIX, this.distribution);
    }


    private static List<String> sortedUniqueValues(Set<String> values) {
        Set<String> uniqueValues = Sets.newHashSet(values);
        List<String> valueList = Lists.newArrayList(uniqueValues);
        Collections.sort(valueList);
        return Collections.unmodifiableList(valueList);
    }
}

