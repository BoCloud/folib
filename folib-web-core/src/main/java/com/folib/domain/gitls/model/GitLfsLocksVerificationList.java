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

