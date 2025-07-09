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

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.Objects;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class HelmDependencyMetadata {
    public String name;

    @JsonProperty("version")
    public String version;

    public String repository;

    public String alias;

    public String condition;

    public List<String> tags;

    public List<Object> importValues;

    public HelmDependencyMetadata() {}

    public HelmDependencyMetadata(@Nonnull String name, @Nonnull String version, @Nonnull String repository) {
        this.name = name;
        this.version = version;
        this.repository = repository;
    }

    public HelmDependencyMetadata(@Nonnull String name, @Nonnull String version, @Nonnull String repository, @Nullable String alias, @Nullable String condition, @Nullable List<String> tags, @Nullable List<Object> importValues) {
        this.name = name;
        this.version = version;
        this.repository = repository;
        this.alias = alias;
        this.condition = condition;
        this.tags = tags;
        this.importValues = importValues;
    }

    public String toString() {
        return this.name + ":" + this.name;
    }

    public boolean equals(Object o) {
        HelmDependencyMetadata that;
        if (this == o)
            return true;
        if (o instanceof HelmDependencyMetadata) {
            that = (HelmDependencyMetadata)o;
        } else {
            return false;
        }
        return (Objects.equals(this.name, that.name) &&
                Objects.equals(this.version, that.version) &&
                Objects.equals(this.repository, that.repository) &&
                Objects.equals(this.condition, that.condition) &&
                Objects.equals(this.importValues, that.importValues) &&
                Objects.equals(this.alias, that.alias) &&
                Objects.equals(this.tags, that.tags));
    }

    public int hashCode() {
        int result = (this.name != null) ? this.name.hashCode() : 0;
        result = 31 * result + ((this.version != null) ? this.version.hashCode() : 0);
        result = 31 * result + ((this.repository != null) ? this.repository.hashCode() : 0);
        result = 31 * result + ((this.condition != null) ? this.condition.hashCode() : 0);
        result = 31 * result + ((this.tags != null) ? this.tags.hashCode() : 0);
        result = 31 * result + ((this.alias != null) ? this.alias.hashCode() : 0);
        result = 31 * result + ((this.importValues != null) ? this.importValues.hashCode() : 0);
        return result;
    }
}
