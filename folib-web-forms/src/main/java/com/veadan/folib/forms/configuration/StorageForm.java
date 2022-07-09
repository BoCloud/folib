package com.veadan.folib.forms.configuration;

import com.veadan.folib.validation.configuration.UniqueStorage;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 * @author Veadan
 */
public class StorageForm
{

    @NotEmpty(message = "An id must be specified.")
    @UniqueStorage(groups = NewStorage.class, message = "The storage id already exists.")
    @Pattern(regexp = "[a-zA-Z0-9\\-\\_\\.]+")
    private String id;

    private String basedir;

    @Valid
    private List<RepositoryForm> repositories;

    public String getId()
    {
        return id;
    }

    public void setId(final String id)
    {
        this.id = id;
    }

    public String getBasedir()
    {
        return basedir;
    }

    public void setBasedir(final String basedir)
    {
        this.basedir = basedir;
    }

    public List<RepositoryForm> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(final List<RepositoryForm> repositories)
    {
        this.repositories = repositories;
    }

    public interface NewStorage
            extends Serializable
    {
        // validation group marker interface for new storages.
    }

    public interface ExistingStorage
            extends Serializable
    {
        // validation group marker interface for existing storages.
    }

}
