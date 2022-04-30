package com.veadan.folib.controllers.users.support;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryAccessModelOutput
{

    private String storageId;

    private String repositoryId;

    private String path;

    private Collection<String> privileges = new ArrayList<>();

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
