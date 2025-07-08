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
