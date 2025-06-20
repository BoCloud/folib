package com.veadan.folib.model;

/**
 * @author leipenghui
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

