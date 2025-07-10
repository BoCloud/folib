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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.html.HtmlEscapers;
import lombok.*;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nonnull;
import java.util.Objects;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PypiIndexEntry implements Comparable<PypiIndexEntry> {
    private static final String LINK_TEMPLATE = "<a href=\"%s\"%s rel=\"%s\">%s</a>";

    private String name;

    private String link;

    @Override
    @Generated
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof PypiIndexEntry)) {
            return false;
        }
        PypiIndexEntry other = (PypiIndexEntry) o;
        if (!other.canEqual(this)) {
            return false;
        }
        Object this$name = getName(), other$name = other.getName();
        return !((this$name == null) ? (other$name != null) : !this$name.equals(other$name));
    }

    @Generated
    protected boolean canEqual(Object other) {
        return other instanceof PypiIndexEntry;
    }

    @Generated
    public void setName(String name) {
        this.name = name;
    }

    @Generated
    public void setLink(String link) {
        this.link = link;
    }

    @Generated
    public String getName() {
        return this.name;
    }

    @Generated
    public String getLink() {
        return this.link;
    }

    @Override
    public String toString() {
        return String.format("<a href=\"%s\">%s</a>", this.link, this.name);
    }

    private void appendNonEmptyPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        if (StringUtils.isNotEmpty(propertyValue)) {
            appendPropertyToStringBuilder(stringBuilder, propertyName, propertyValue);
        }
    }

    private void appendNonNullPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        if (propertyValue != null) {
            appendPropertyToStringBuilder(stringBuilder, propertyName, propertyValue);
        }
    }

    private void appendPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        String propertyEscaped = HtmlEscapers.htmlEscaper().escape(propertyValue);
        stringBuilder.append(" ").append(propertyName).append("=\"").append(propertyEscaped).append("\"");
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public int compareTo(@Nonnull PypiIndexEntry o) {
        if (this.name == null) {
            return -1;
        }
        if (o.name == null) {
            return 1;
        }
        return this.name.compareTo(o.name);
    }
}

