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

import java.beans.ConstructorProperties;
import javax.annotation.Nullable;
import lombok.Generated;

public class GitLfsCreateLock {
    private String path;

    @Nullable
    private GitLfsName ref;

    private String owner;

    @Generated
    public GitLfsCreateLock() {}

    @ConstructorProperties({"path", "ref"})
    @Generated
    public GitLfsCreateLock(String path, @Nullable GitLfsName ref) {
        this.path = path;
        this.ref = ref;
    }

    @Generated
    public void setPath(String path) {
        this.path = path;
    }

    @Generated
    public void setRef(@Nullable GitLfsName ref) {
        this.ref = ref;
    }

    @Generated
    public String getPath() {
        return this.path;
    }

    @Nullable
    @Generated
    public GitLfsName getRef() {
        return this.ref;
    }
    @Generated
    public void setOwner(String owner) {
        this.owner = owner;
    }

    @Nullable
    @Generated
    public String getOwner() {
        return owner;
    }
}
