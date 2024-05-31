package com.veadan.folib.domain.gitls.model;

import java.beans.ConstructorProperties;

public class GitLfsLocksVerification {

    @ConstructorProperties({"cursor", "limit", "ref"})
    public GitLfsLocksVerification(int cursor, int limit, GitLfsName ref) {
        this.cursor = cursor;
        this.limit = limit;
        this.ref = ref;
    }

    
    public void setCursor(int cursor) {
        this.cursor = cursor;
    }

    
    public void setLimit(int limit) {
        this.limit = limit;
    }

    
    public void setRef(GitLfsName ref) {
        this.ref = ref;
    }

    private int cursor = 0;

    
    public int getCursor() {
        return this.cursor;
    }
    private int limit = 0;

    private GitLfsName ref;

    public int getLimit() {
        return this.limit;
    }

    public GitLfsName getRef() {
        return this.ref;
    }
    public GitLfsLocksVerification() {}
}
