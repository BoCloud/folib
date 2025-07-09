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
import lombok.Generated;
public class GitLfsLock {

    private String id;

    private String path;

    private String lockedAt;

    private GitLfsName owner;

    private GitLfsName ref;

    @Generated
    public GitLfsLock() {}

    @ConstructorProperties({"id", "path", "lockedAt", "owner", "ref"})
    @Generated
    public GitLfsLock(String id, String path, String lockedAt, GitLfsName owner, GitLfsName ref) {
        this.id = id;
        this.path = path;
        this.lockedAt = lockedAt;
        this.owner = owner;
        this.ref = ref;
    }

    @Generated
    public static GitLfsLockBuilder builder() {
        return new GitLfsLockBuilder();
    }

    @Generated
    public static class GitLfsLockBuilder {
        @Generated
        private String id;

        @Generated
        private String path;

        @Generated
        private String lockedAt;

        @Generated
        private GitLfsName owner;

        @Generated
        private GitLfsName ref;

        @Generated
        public GitLfsLockBuilder id(String id) {
            this.id = id;
            return this;
        }

        @Generated
        public GitLfsLockBuilder path(String path) {
            this.path = path;
            return this;
        }

        @Generated
        public GitLfsLockBuilder lockedAt(String lockedAt) {
            this.lockedAt = lockedAt;
            return this;
        }

        @Generated
        public GitLfsLockBuilder owner(GitLfsName owner) {
            this.owner = owner;
            return this;
        }

        @Generated
        public GitLfsLockBuilder ref(GitLfsName ref) {
            this.ref = ref;
            return this;
        }

        @Generated
        public GitLfsLock build() {
            return new GitLfsLock(this.id, this.path, this.lockedAt, this.owner, this.ref);
        }

        @Generated
        public String toString() {
            return "GitLfsLock.GitLfsLockBuilder(id=" + this.id + ", path=" + this.path + ", lockedAt=" + this.lockedAt + ", owner=" + this.owner + ", ref=" + this.ref + ")";
        }
    }

    @Generated
    public void setId(String id) {
        this.id = id;
    }

    @Generated
    public void setPath(String path) {
        this.path = path;
    }

    @Generated
    public void setLockedAt(String lockedAt) {
        this.lockedAt = lockedAt;
    }

    @Generated
    public void setOwner(GitLfsName owner) {
        this.owner = owner;
    }

    @Generated
    public void setRef(GitLfsName ref) {
        this.ref = ref;
    }

    @Generated
    public String getId() {
        return this.id;
    }

    @Generated
    public String getPath() {
        return this.path;
    }

    @Generated
    public String getLockedAt() {
        return this.lockedAt;
    }

    @Generated
    public GitLfsName getOwner() {
        return this.owner;
    }

    @Generated
    public GitLfsName getRef() {
        return this.ref;
    }

    public static class Root {
        private GitLfsLock lock;

        @Generated
        public Root() {}

        @ConstructorProperties({"lock"})
        @Generated
        public Root(GitLfsLock lock) {
            this.lock = lock;
        }

        @Generated
        public GitLfsLock getLock() {
            return this.lock;
        }
    }
}
