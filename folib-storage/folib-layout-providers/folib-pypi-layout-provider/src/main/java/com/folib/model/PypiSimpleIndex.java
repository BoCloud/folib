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

/**
 * @author veadan
 * @date 2024/7/3
 **/

import com.google.common.collect.Sets;
import lombok.Generated;
import org.apache.commons.collections4.CollectionUtils;

import java.util.SortedSet;

public class PypiSimpleIndex {
    public static final String INDEX_PREAMBLE = "<!DOCTYPE html>\n<html><head><title>Simple Index</title><meta name=\"api-version\" value=\"2\" /></head><body>\n";

    public static final String INDEX_APPENDIX = "</body></html>";

    private SortedSet<PypiIndexEntry> entries;

    @Generated
    public void setEntries(SortedSet<PypiIndexEntry> entries) {
        this.entries = entries;
    }

    @Generated
    protected boolean canEqual(Object other) {
        return other instanceof PypiSimpleIndex;
    }

    @Generated
    public SortedSet<PypiIndexEntry> getEntries() {
        return this.entries;
    }

    public PypiSimpleIndex() {
        this(null);
    }

    public PypiSimpleIndex(SortedSet<PypiIndexEntry> entries) {
        this.entries = (entries != null) ? entries : Sets.newTreeSet();
    }

    @Override
    public String toString() {
        if (CollectionUtils.isEmpty(this.entries)) {
            return "";
        }
        String packageName = "";
        if (CollectionUtils.isNotEmpty(this.entries)) {
            packageName = this.entries.first().getName();
        }
        StringBuilder sb = new StringBuilder(String.format("<!DOCTYPE html>\n<html><head><title>Simple Index</title><meta name=\"api-version\" value=\"2\" />" +
                "<title>Links for %s</title>\n</head><body><h1>Links for %s</h1><br>\n", packageName, packageName));
        for (PypiIndexEntry entry : this.entries) {
            sb.append(entry.toString()).append("<br>");
        }
        sb.append("</body></html>");
        return sb.toString();
    }
}

