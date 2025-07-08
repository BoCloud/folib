package com.folib.controllers.adapter.jfrog.dto;

import lombok.Data;

/**
 * docker 制品晋级的参数接收实体
 */
@Data
public class DockerCopyDto {
    private String targetStorageId;
    // The target repository for the move or copy
    private String  targetRepo;
    // The image name to promote
    private String dockerRepository ;
    // An optional docker repository name, if null, will use the same name as 'dockerRepository'
    private String  targetDockerRepository;
    // An optional tag name to promote, if null - the entire docker repository will be promoted. Available from v4.10.
    private String  tag ;
    // An optional target tag to assign the image after promotion, if null - will use the same tag
    private String  targetTag ;
    //  是否拷贝为false
    private Boolean  copy= false;
}
