package com.folib.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSetter;
import java.io.Serializable;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Generated;
import lombok.NoArgsConstructor;

/**
 * @author pj
 * @date 2021/12/13 16:09
 * @description cargo 依赖包元数据
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CargoDependencyMetadata implements Serializable {
    public static final boolean DEFAULT_FEATURES_DEFAULT_VALUE = true;

    public static final boolean OPTIONAL_DEFAULT_VALUE = false;

    private String name;

    private String versionReq;

    private List<String> features;

    private boolean optional;

    private boolean defaultFeatures;

    private String target;

    private String kind;

    private String packageExplicitName;

    private String registry;



    @Generated
    public static CargoDependencyMetadataBuilder builder() {
        return new CargoDependencyMetadataBuilder();
    }

    @Generated
    public static class CargoDependencyMetadataBuilder {
        @Generated
        private String name;

        @Generated
        private String versionReq;

        @Generated
        private List<String> features;

        @Generated
        private boolean optional;

        @Generated
        private boolean defaultFeatures;

        @Generated
        private String target;

        @Generated
        private String kind;

        @Generated
        private String packageExplicitName;

        @Generated
        private String registry;

        @Generated
        public CargoDependencyMetadataBuilder name(String name) {
            this.name = name;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder versionReq(String versionReq) {
            this.versionReq = versionReq;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder features(List<String> features) {
            this.features = features;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder optional(boolean optional) {
            this.optional = optional;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder defaultFeatures(boolean defaultFeatures) {
            this.defaultFeatures = defaultFeatures;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder target(String target) {
            this.target = target;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder kind(String kind) {
            this.kind = kind;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder packageExplicitName(String packageExplicitName) {
            this.packageExplicitName = packageExplicitName;
            return this;
        }

        @Generated
        public CargoDependencyMetadataBuilder registry(String registry) {
            this.registry = registry;
            return this;
        }

        @Generated
        public CargoDependencyMetadata build() {
            return new CargoDependencyMetadata(this.name, this.versionReq, this.features, this.optional, this.defaultFeatures, this.target, this.kind, this.packageExplicitName, this.registry);
        }
    }

    @Generated
    public String getName() {
        return this.name;
    }

    @Generated
    public String getVersionReq() {
        return this.versionReq;
    }

    @Generated
    public List<String> getFeatures() {
        return this.features;
    }

    @Generated
    public boolean isOptional() {
        return this.optional;
    }

    @Generated
    public boolean isDefaultFeatures() {
        return this.defaultFeatures;
    }

    @Generated
    public String getTarget() {
        return this.target;
    }

    @Generated
    public String getKind() {
        return this.kind;
    }

    @Generated
    public String getPackageExplicitName() {
        return this.packageExplicitName;
    }

    @Generated
    public String getRegistry() {
        return this.registry;
    }
    @JsonSetter("explicit_name_in_toml")
    public void setExplicitNameInToml(String packageRename) {
        this.packageExplicitName = packageRename;
    }
}

