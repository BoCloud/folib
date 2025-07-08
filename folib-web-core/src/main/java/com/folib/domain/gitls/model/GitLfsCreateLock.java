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
