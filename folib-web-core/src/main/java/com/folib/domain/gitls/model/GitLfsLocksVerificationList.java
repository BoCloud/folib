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


import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import java.util.LinkedList;
import java.util.List;
import lombok.Generated;


@JsonNaming(PropertyNamingStrategy.SnakeCaseStrategy.class)
public class GitLfsLocksVerificationList {
    @Generated
    public void setOurs(List<GitLfsLock> ours) {
        this.ours = ours;
    }

    @Generated
    public void setTheirs(List<GitLfsLock> theirs) {
        this.theirs = theirs;
    }

    @Generated
    public void setNextCursor(String nextCursor) {
        this.nextCursor = nextCursor;
    }

    @Generated
    public List<GitLfsLock> getOurs() {
        return this.ours;
    }

    @Generated
    public List<GitLfsLock> getTheirs() {
        return this.theirs;
    }

    @Generated
    public String getNextCursor() {
        return this.nextCursor;
    }

    private List<GitLfsLock> ours = new LinkedList<>();

    private List<GitLfsLock> theirs = new LinkedList<>();

    private String nextCursor;
}

