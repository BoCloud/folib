package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/9 10:13
 * @since x.x.x
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactUploadAdapterJfrogDto 
{
    /*
    {
  "repo" : "Cocoapad-Local",
  "path" : "/JSONKit/dddddddd1/JSONKit-v1.3.tar.gz-2121",
  "created" : "2023-10-08T08:35:28.712Z",
  "createdBy" : "admin",
  "downloadUri" : "http://10.10.33.149:8081/artifactory/Cocoapad-Local/JSONKit/dddddddd1/JSONKit-v1.3.tar.gz-2121",
  "mimeType" : "application/octet-stream",
  "size" : "31111",
  "checksums" : {
    "sha1" : "7e890583e2685e3353183c3981989b02445dacaa",
    "md5" : "d1dc609bac7097a8b512db038f4ac9f8",
    "sha256" : "bf90df4b6e7438094acd3add951f55c310a8e708a8589dc05995f9bd7cd8f417"
  },
  "originalChecksums" : {
    "sha256" : "bf90df4b6e7438094acd3add951f55c310a8e708a8589dc05995f9bd7cd8f417"
  },
  "uri" : "http://10.10.33.149:8081/artifactory/Cocoapad-Local/JSONKit/dddddddd1/JSONKit-v1.3.tar.gz-2121"
}
    * */
    private String repo;
    private String path;
    private String created;
    private String createdBy;
    private String downloadUri;
    private String mimeType;
    private String size;
    private Checksums checksums;
    private OriginalChecksums originalChecksums;
    private String uri;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class Checksums
    {
        private String sha1;
        private String md5;
        private String sha256;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class OriginalChecksums
    {
        private String sha256;
    }
}
