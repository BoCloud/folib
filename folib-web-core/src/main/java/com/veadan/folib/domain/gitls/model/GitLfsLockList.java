package com.veadan.folib.domain.gitls.model;


import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import java.beans.ConstructorProperties;
import java.util.List;

@JsonNaming(PropertyNamingStrategy.SnakeCaseStrategy.class)
public class GitLfsLockList {
    private List<GitLfsLock> locks;

    private String nextCursor;

    public GitLfsLockList() {}

    @ConstructorProperties({"locks", "nextCursor"})
    public GitLfsLockList(List<GitLfsLock> locks, String nextCursor) {
        this.locks = locks;
        this.nextCursor = nextCursor;
    }


    public void setLocks(List<GitLfsLock> locks) {
        this.locks = locks;
    }


    public void setNextCursor(String nextCursor) {
        this.nextCursor = nextCursor;
    }


    public List<GitLfsLock> getLocks() {
        return this.locks;
    }

    public String getNextCursor() {
        return this.nextCursor;
    }
}

