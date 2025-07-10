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

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Generated;

public class MlCommitInfo {
    private String commitUrl;

    private String commitMessage;

    private String commitDescription;

    private String commitOid;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String pullRequestUrl;


    public MlCommitInfo() {}

    public MlCommitInfo(String commitUrl, String commitMessage, String commitDescription, String commitOid, String pullRequestUrl) {
        this.commitUrl = commitUrl;
        this.commitMessage = commitMessage;
        this.commitDescription = commitDescription;
        this.commitOid = commitOid;
        this.pullRequestUrl = pullRequestUrl;
    }


    public String getCommitUrl() {
        return this.commitUrl;
    }


    public String getCommitMessage() {
        return this.commitMessage;
    }


    public String getCommitDescription() {
        return this.commitDescription;
    }


    public String getCommitOid() {
        return this.commitOid;
    }

    public String getPullRequestUrl() {
        return this.pullRequestUrl;
    }
}

