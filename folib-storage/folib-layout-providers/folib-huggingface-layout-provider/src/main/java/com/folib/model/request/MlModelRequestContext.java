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
package com.folib.model.request;

import java.beans.ConstructorProperties;
import javax.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;


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
