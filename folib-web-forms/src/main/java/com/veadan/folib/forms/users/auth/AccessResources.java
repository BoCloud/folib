package com.veadan.folib.forms.users.auth;

import lombok.Data;

import javax.validation.constraints.NotEmpty;

/**
 * @author veadan
 */
@Data
public class AccessResources
{
    private String resourceId;
    @NotEmpty(message = "A storage id must be specified.")
    private String storageId;

    @NotEmpty(message = "A repository id must be specified.")
    private String repositoryId;

    private String path;

    //private boolean wildcard;

}
