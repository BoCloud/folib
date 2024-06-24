package com.veadan.folib.domain.gitls.model;

import java.beans.ConstructorProperties;
import lombok.Generated;

public class GitLfsName {
    private String name;

    @Generated
    public GitLfsName() {}

    @ConstructorProperties({"name"})
    @Generated
    public GitLfsName(String name) {
        this.name = name;
    }

    @Generated
    public void setName(String name) {
        this.name = name;
    }

    @Generated
    public String getName() {
        return this.name;
    }
}
