package com.folib.forms.users.auth;

import lombok.Data;

import javax.validation.constraints.NotEmpty;
import java.util.Collection;

/**
 * @author veadan
 */
@Data
public class RepositoryAccessModelForm
{

    private Long resourceId;
    @NotEmpty(message = "A storage id must be specified.")
    private String storageId;

    @NotEmpty(message = "A repository id must be specified.")
    private String repositoryId;

    private String path;

    @NotEmpty(message = "A collection of privileges must be specified.")
    private Collection<String> privileges;

    private boolean wildcard;

}
