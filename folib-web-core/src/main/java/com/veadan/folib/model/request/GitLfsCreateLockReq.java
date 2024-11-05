package com.veadan.folib.model.request;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.Generated;
import lombok.experimental.Accessors;

import javax.annotation.Nullable;
import java.beans.ConstructorProperties;

@Data
@Accessors(chain = true)
@ApiModel(description = "git lfs create lock")
public class GitLfsCreateLockReq {

    private String path;

    @Nullable
    private GitLfsName ref;

    @Generated
    public GitLfsCreateLockReq() {
    }

    @ConstructorProperties({"path", "ref"})
    @Generated
    public GitLfsCreateLockReq(String path, @Nullable GitLfsName ref) {
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

    public static class GitLfsName {
        private String name;

        @Generated
        public GitLfsName() {
        }

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
}
