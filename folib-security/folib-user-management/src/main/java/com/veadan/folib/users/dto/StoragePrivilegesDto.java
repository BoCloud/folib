package com.veadan.folib.users.dto;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.veadan.folib.users.domain.Privileges;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 
 * @author Veadan
 */
@Data
@AllArgsConstructor
public class StoragePrivilegesDto
        implements Serializable, StoragePrivileges
{

    private Set<RepositoryPrivilegesDto> repositoryPrivileges = new LinkedHashSet<>();

    @JsonProperty(value = "storageId")
    private String storageId;

    private Set<Privileges> storagePrivileges = new LinkedHashSet<>();

    public StoragePrivilegesDto()
    {
    }

    @JsonCreator
    public StoragePrivilegesDto(@JsonProperty(value = "storageId", required = true) String storageId,@JsonProperty(value = "storagePrivileges", required = false) Set<Privileges> storagePrivileges)
    {
        this.storageId = storageId;
        this.storagePrivileges = storagePrivileges;
    }

    @Override
    public Set<RepositoryPrivilegesDto> getRepositoryPrivileges()
    {
        return repositoryPrivileges;
    }

    @Override
    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(final String storageId)
    {
        this.storageId = storageId;
    }

    public Optional<RepositoryPrivilegesDto> getRepositoryPrivileges(final String repositoryId)
    {
        return repositoryPrivileges.stream().filter(r -> r.getRepositoryId().equals(repositoryId)).findFirst();
    }

    @Override
    public Set<Privileges> getStoragePrivileges() {
        return storagePrivileges;
    }
}
