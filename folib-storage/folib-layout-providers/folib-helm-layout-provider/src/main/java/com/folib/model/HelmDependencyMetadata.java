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
