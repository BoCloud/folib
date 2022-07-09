package com.veadan.folib.storage;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * @author mtodorov
 * @author Veadan
 */
@JsonRootName("storage")
public class StorageDto
        implements Serializable, Storage
{
    private String id;
    
    private String basedir;

    private Map<String, RepositoryDto> repositories = new LinkedHashMap<>();

    public StorageDto()
    {
    }

    @JsonCreator
    public StorageDto(@JsonProperty(value = "id", required = true) String id)
    {
        this.id = id;
    }

    public boolean containsRepository(String repository)
    {
        return getRepositories().containsKey(repository);
    }

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getBasedir()
    {
        return basedir;
    }

    public void setBasedir(String basedir)
    {
        this.basedir = basedir;
    }

    @Override
    public Map<String, ? extends Repository> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(Map<String, RepositoryDto> repositories)
    {
        this.repositories = repositories;
    }

    public void addRepository(RepositoryDto repository)
    {
        repositories.put(repository.getId(), repository);
    }

    public RepositoryDto getRepository(String repositoryId)
    {
        return repositories.get(repositoryId);
    }

    public void removeRepository(String repositoryId)
    {
        repositories.remove(repositoryId);
    }

    public boolean hasRepositories()
    {
        return !CollectionUtils.isEmpty(repositories);
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("Storage{");
        sb.append("\n\t\tid='")
          .append(id)
          .append('\'');
        sb.append(", \n\t\tbasedir='").append(basedir).append('\'');
        sb.append(", \n\t\trepositories=").append(repositories);
        sb.append('}');
        return sb.toString();
    }
    
}
