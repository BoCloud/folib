package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 *
 * @author veadan
 * @date 2023/10/9 10:13
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactUploadAdapterJfrogDto 
{
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
    @Accessors(chain = true)
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
    @Accessors(chain = true)
    public static class OriginalChecksums
    {
        private String sha256;
    }
}
