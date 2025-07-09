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
package com.folib.domain.gitls.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Maps;
import java.beans.ConstructorProperties;
import java.util.Map;
import lombok.Generated;

@JsonSerialize(using = GitLfsJsonResponseSerializer.class)
@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonIgnoreProperties(ignoreUnknown = true)
public class GitLfsJson {
    @Generated
    public void setOid(String oid) {
        this.oid = oid;
    }

    @Generated
    public void setSize(Long size) {
        this.size = size;
    }

    @Generated
    public void setError(GitLfsJsonError error) {
        this.error = error;
    }

    @Generated
    public void setSelfLink(String selfLink) {
        this.selfLink = selfLink;
    }

    @Generated
    public void setDownloadLink(String downloadLink) {
        this.downloadLink = downloadLink;
    }

    @Generated
    public void setUploadLink(String uploadLink) {
        this.uploadLink = uploadLink;
    }

    @Generated
    public void setVerifyLink(String verifyLink) {
        this.verifyLink = verifyLink;
    }

    @Generated
    public void setDownloadHeaders(Map<String, String> downloadHeaders) {
        this.downloadHeaders = downloadHeaders;
    }

    @Generated
    public void setUploadHeaders(Map<String, String> uploadHeaders) {
        this.uploadHeaders = uploadHeaders;
    }

    @Generated
    public void setVerifyHeaders(Map<String, String> verifyHeaders) {
        this.verifyHeaders = verifyHeaders;
    }

    private String oid = null;

    @Generated
    public String getOid() {
        return this.oid;
    }

    private Long size = null;

    @Generated
    public Long getSize() {
        return this.size;
    }

    private GitLfsJsonError error = null;

    private String selfLink;

    private String downloadLink;

    private String uploadLink;

    private String verifyLink;

    @Generated
    public GitLfsJsonError getError() {
        return this.error;
    }

    @Generated
    public String getSelfLink() {
        return this.selfLink;
    }

    @Generated
    public String getDownloadLink() {
        return this.downloadLink;
    }

    @Generated
    public String getUploadLink() {
        return this.uploadLink;
    }

    @Generated
    public String getVerifyLink() {
        return this.verifyLink;
    }

    private Map<String, String> downloadHeaders = Maps.newHashMap();

    @Generated
    public Map<String, String> getDownloadHeaders() {
        return this.downloadHeaders;
    }

    private Map<String, String> uploadHeaders = Maps.newHashMap();

    public Map<String, String> getUploadHeaders() {
        return this.uploadHeaders;
    }

    private Map<String, String> verifyHeaders = Maps.newHashMap();


    public Map<String, String> getVerifyHeaders() {
        return this.verifyHeaders;
    }

    public GitLfsJson(String oid, long size) {
        this.oid = oid;
        this.size = Long.valueOf(size);
    }

    public GitLfsJson(GitLfsJson that) {
        this.oid = that.oid;
        this.size = that.size;
        this.downloadHeaders = that.downloadHeaders;
        this.uploadHeaders = that.uploadHeaders;
        this.verifyHeaders = that.verifyHeaders;
    }

    @JsonIgnore
    public GitLfsJson registerError(int code, String message) {
        this.error = new GitLfsJsonError(code, message);
        return this;
    }

    public String toString() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
        try {
            return objectMapper.writeValueAsString(this);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    @Generated
    public GitLfsJson() {}

    public static class GitLfsJsonError {
        private int code;

        private String message;

        @ConstructorProperties({"code", "message"})
        public GitLfsJsonError(int code, String message) {
            this.code = code;
            this.message = message;
        }

        public int getCode() {
            return this.code;
        }

        public String getMessage() {
            return this.message;
        }
    }
}
