package com.veadan.folib.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactSliceDownloadInfoRes {
    private String storageId;
    private String repositoryId;
    private String path;
    
    private Boolean usedSlice;
    private String artifactMd5;
    private List<DownloadPartInfo> downloadPartList; 
    
    
    @Data
    @Accessors(chain = true)
    public static class DownloadPartInfo implements Cloneable {
        private String downloadUri;
        private String downloadUrl;
        private Long size;
        private String temId;

        @Override
        public DownloadPartInfo clone() {
            try {
                return (DownloadPartInfo) super.clone();
            } catch (CloneNotSupportedException e) {
                throw new AssertionError();
            }
        }
    }
}
