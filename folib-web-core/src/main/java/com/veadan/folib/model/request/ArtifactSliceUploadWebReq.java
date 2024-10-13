package com.veadan.folib.model.request;

import lombok.Data;


@Data
public class ArtifactSliceUploadWebReq extends ArtifactSliceUploadReq {

    private String imageTag;
    private String fileType;
    private String baseUrl;
    private String token;
    private boolean isUnzip;
    private String  fileName;
    private String originalFilename;
}
