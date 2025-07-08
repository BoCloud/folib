package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Collection;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryAccessModelDTO
{

    private String resourceId;

    private String storageId;

    private String repositoryId;

    private String path;

    private Collection<String> privileges;

    private boolean wildcard;

}
