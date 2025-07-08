package com.folib.domain.gitls.model;

import java.beans.ConstructorProperties;

public class GitLfsDeleteLock {

    private boolean force;

    private GitLfsName ref;


    public GitLfsDeleteLock() {
    }

    @ConstructorProperties({"force", "ref"})
    public GitLfsDeleteLock(boolean force, GitLfsName ref) {
        this.force = force;
        this.ref = ref;
    }

    public void setForce(boolean force) {
        this.force = force;
    }

    public void setRef(GitLfsName ref) {
        this.ref = ref;
    }

    public boolean isForce() {
        return this.force;
    }

    public GitLfsName getRef() {
        return this.ref;
    }

}
