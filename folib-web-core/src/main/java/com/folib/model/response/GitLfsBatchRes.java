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
package com.folib.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel(description = "git lfs batch res")
public class GitLfsBatchRes {

    private List<LfsObjectRes> objects;

    public static class LfsObjectRes {
        private String oid;
        private long size;
        @JsonProperty("_links")
        private LfsLinksRes links;

        public String getOid() {
            return oid;
        }

        public long getSize() {
            return size;
        }

        public LfsLinksRes getLinks() {
            return links;
        }

        public void setOid(String oid) {
            this.oid = oid;
        }

        public void setSize(long size) {
            this.size = size;
        }

        public void setLinks(LfsLinksRes links) {
            this.links = links;
        }
    }

    public static class LfsLinksRes {
        @JsonInclude(JsonInclude.Include.NON_NULL)
        private LfsUploadRes upload;
        @JsonInclude(JsonInclude.Include.NON_NULL)
        private LfsDownloadRes download;

        public LfsUploadRes getUpload() {
            return upload;
        }

        public LfsDownloadRes getDownload() {
            return download;
        }

        public void setUpload(LfsUploadRes upload) {
            this.upload = upload;
        }

        public void setDownload(LfsDownloadRes download) {
            this.download = download;
        }

    }

    public static class LfsUploadRes {

        private String href;
        private LfsHeaderRes header;

        public String getHref() {
            return href;
        }

        public LfsHeaderRes getHeader() {
            return header;
        }

        public void setHref(String href) {
            this.href = href;
        }

        public void setHeader(LfsHeaderRes header) {
            this.header = header;
        }

    }

    public static class LfsDownloadRes {
        private String href;
        @JsonInclude(JsonInclude.Include.NON_NULL)
        private LfsErrorRes error;

        public String getHref() {
            return href;
        }

        public LfsErrorRes getError() {
            return error;
        }

        public void setHref(String href) {
            this.href = href;
        }

        public void setError(LfsErrorRes error) {
            this.error = error;
        }

    }

    public static class LfsHeaderRes {
        @JsonProperty("Authorization")
        private String authorization;
        @JsonProperty("X-Checksum-Sha256")
        private String sha256;

        public String getAuthorization() {
            return authorization;
        }

        public String getSha256() {
            return sha256;
        }

        public void setAuthorization(String authorization) {
            this.authorization = authorization;
        }

        public void setSha256(String sha256) {
            this.sha256 = sha256;
        }
    }

    public static class LfsErrorRes {
        private Integer code;
        private String message;

        public LfsErrorRes(Integer code, String message) {
            this.code = code;
            this.message = message;
        }

        public LfsErrorRes() {
        }

        public Integer getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        public void setCode(Integer code) {
            this.code = code;
        }

        public void setMessage(String message) {
            this.message = message;
        }
}
}
