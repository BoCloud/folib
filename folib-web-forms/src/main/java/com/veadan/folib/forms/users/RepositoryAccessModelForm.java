package com.veadan.folib.forms.users;

import javax.validation.constraints.NotEmpty;
import java.util.Collection;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryAccessModelForm
{

    @NotEmpty(message = "A storage id must be specified.")
    private String storageId;

    @NotEmpty(message = "A repository id must be specified.")
    private String repositoryId;

    private String path;

    @NotEmpty(message = "A collection of privileges must be specified.")
    private Collection<String> privileges;

    private boolean wildcard;

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(final String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(final String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public String getPath()
    {
        return path;
    }

    public void setPath(final String path)
    {
        this.path = path;
    }

    public Collection<String> getPrivileges()
    {
        return privileges;
    }

    public void setPrivileges(final Collection<String> privileges)
    {
        this.privileges = privileges;
    }

    public boolean isWildcard()
    {
        return wildcard;
    }

    public void setWildcard(final boolean wildcard)
    {
        this.wildcard = wildcard;
    }
}
