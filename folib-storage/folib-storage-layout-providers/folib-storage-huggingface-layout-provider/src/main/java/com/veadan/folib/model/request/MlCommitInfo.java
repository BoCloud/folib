package com.veadan.folib.model.request;

import java.beans.ConstructorProperties;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Generated;

public class MlCommitInfo {
    private String commitUrl;

    private String commitMessage;

    private String commitDescription;

    private String commitOid;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String pullRequestUrl;


    public MlCommitInfo() {}

    public MlCommitInfo(String commitUrl, String commitMessage, String commitDescription, String commitOid, String pullRequestUrl) {
        this.commitUrl = commitUrl;
        this.commitMessage = commitMessage;
        this.commitDescription = commitDescription;
        this.commitOid = commitOid;
        this.pullRequestUrl = pullRequestUrl;
    }


    public String getCommitUrl() {
        return this.commitUrl;
    }


    public String getCommitMessage() {
        return this.commitMessage;
    }


    public String getCommitDescription() {
        return this.commitDescription;
    }


    public String getCommitOid() {
        return this.commitOid;
    }

    public String getPullRequestUrl() {
        return this.pullRequestUrl;
    }
}

