package com.veadan.folib.domain.gitls.model;

import java.beans.ConstructorProperties;
import lombok.Generated;

public class Root {
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
