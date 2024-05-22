package com.veadan.folib.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel(description = "git lfs batch res")
public class GitLfsBatchRes {

    private List<LfsObjectRes> objects;

    public static class LfsObjectRes {
        private String oid;
        private long size;
        @JsonProperty("_links")
        private LfsLinksRes links;

        public String getOid() {
            return oid;
        }

        public long getSize() {
            return size;
        }

        public LfsLinksRes getLinks() {
            return links;
        }

        public void setOid(String oid) {
            this.oid = oid;
        }

        public void setSize(long size) {
            this.size = size;
        }

        public void setLinks(LfsLinksRes links) {
            this.links = links;
        }
    }

    public static class LfsLinksRes {
        @JsonInclude(JsonInclude.Include.NON_NULL)
        private LfsUploadRes upload;
        @JsonInclude(JsonInclude.Include.NON_NULL)
        private LfsDownloadRes download;

        public LfsUploadRes getUpload() {
            return upload;
        }

        public LfsDownloadRes getDownload() {
            return download;
        }

        public void setUpload(LfsUploadRes upload) {
            this.upload = upload;
        }

        public void setDownload(LfsDownloadRes download) {
            this.download = download;
        }

    }

    public static class LfsUploadRes {

        private String href;
        private LfsHeaderRes header;

        public String getHref() {
            return href;
        }

        public LfsHeaderRes getHeader() {
            return header;
        }

        public void setHref(String href) {
            this.href = href;
        }

        public void setHeader(LfsHeaderRes header) {
            this.header = header;
        }

    }

    public static class LfsDownloadRes {
        private String href;

        public String getHref() {
            return href;
        }

        public void setHref(String href) {
            this.href = href;
        }

    }

    public static class LfsHeaderRes {
        @JsonProperty("Authorization")
        private String authorization;
        @JsonProperty("X-Checksum-Sha256")
        private String sha256;

        public String getAuthorization() {
            return authorization;
        }

        public String getSha256() {
            return sha256;
        }

        public void setAuthorization(String authorization) {
            this.authorization = authorization;
        }

        public void setSha256(String sha256) {
            this.sha256 = sha256;
        }
    }
}
