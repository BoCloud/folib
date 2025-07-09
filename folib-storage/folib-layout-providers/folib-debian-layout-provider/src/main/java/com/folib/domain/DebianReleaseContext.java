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
package com.folib.domain;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.constant.DebianConstant;

import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * @author veadan
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

