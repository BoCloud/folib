package com.veadan.folib.model.request;

import java.beans.ConstructorProperties;
import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;

import lombok.Data;
import lombok.Generated;
import org.apache.commons.lang.StringUtils;

@Data
public class MlModelRequestContext {


    private String storageId;

    private String repositoryId;

    @Nullable
    private String org;

    private String modelName;

    private String revision;

    private String generatedSha1;

    private String versionFolder;

    private String file;

    private String originalRemoteCommit;

    private HttpServletRequest request;

    @ConstructorProperties({"storageId","repositoryId", "org", "modelName", "revision", "generatedSha1", "versionFolder", "file", "originalRemoteCommit", "request"})
    @Generated
    MlModelRequestContext(String storageId,String repositoryId, String org, String modelName, String revision, String generatedSha1, String versionFolder, String file, String originalRemoteCommit, HttpServletRequest request) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.org = org;
        this.modelName = modelName;
        this.revision = revision;
        this.generatedSha1 = generatedSha1;
        this.versionFolder = versionFolder;
        this.file = file;
        this.originalRemoteCommit = originalRemoteCommit;
        this.request = request;
    }

    @Generated
    public static MlModelRequestContextBuilder builder() {
        return new MlModelRequestContextBuilder();
    }

    @Generated
    public static class MlModelRequestContextBuilder {
        @Generated
        private String storageId;
        @Generated
        private String repositoryId;

        @Generated
        private String org;

        @Generated
        private String modelName;

        @Generated
        private String revision;

        @Generated
        private String generatedSha1;

        @Generated
        private String versionFolder;

        @Generated
        private String file;

        @Generated
        private String originalRemoteCommit;

        @Generated
        private HttpServletRequest request;

        @Generated
        public MlModelRequestContextBuilder repositoryId(String repoKey) {
            this.repositoryId = repoKey;
            return this;
        }

        public MlModelRequestContextBuilder storageId(String storageId) {
            this.storageId = storageId;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder org(@Nullable String org) {
            this.org = org;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder modelName(String modelName) {
            this.modelName = modelName;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder revision(String revision) {
            this.revision = revision;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder generatedSha1(String generatedSha1) {
            this.generatedSha1 = generatedSha1;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder versionFolder(String versionFolder) {
            this.versionFolder = versionFolder;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder file(String file) {
            this.file = file;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder originalRemoteCommit(String originalRemoteCommit) {
            this.originalRemoteCommit = originalRemoteCommit;
            return this;
        }

        @Generated
        public MlModelRequestContextBuilder request(HttpServletRequest request) {
            this.request = request;
            return this;
        }

        @Generated
        public MlModelRequestContext build() {
            return new MlModelRequestContext(this.storageId,this.repositoryId, this.org, this.modelName, this.revision, this.generatedSha1, this.versionFolder, this.file, this.originalRemoteCommit, this.request);
        }
    }

    public String modelId() {
        if (StringUtils.isBlank(this.org) && StringUtils.isNotBlank(this.modelName)) {
            return this.modelName;
        }
        return String.join("/",  this.org, this.modelName );
    }


    public void setRevision(String commitHashRevision) {
        if (StringUtils.isNotBlank(commitHashRevision)) {
            this.revision = commitHashRevision;
        }
    }
}
