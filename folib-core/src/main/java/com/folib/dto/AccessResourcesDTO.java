package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessResourcesDTO
{
    private String resourceId;
    private String storageId;

    private String repositoryId;

    private String path;

    //private boolean wildcard;

}
