package com.veadan.folib.domain.huggingface.model.request;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import lombok.Generated;

@Generated
public class MlModelRequestContextBuilder {

    private String storageId;

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
    public MlModelRequestContextBuilder storageId(String storageId) {
        this.storageId = storageId;
        return this;
    }

    @Generated
    public MlModelRequestContextBuilder repositoryId(String repositoryId) {
        this.repositoryId = repositoryId;
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
