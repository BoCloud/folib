package com.veadan.folib.users.dto;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import com.veadan.folib.users.domain.Privileges;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author 
 * @author Veadan
 * @author veadan
 */
public class RepositoryPrivilegesDto
        implements Serializable, RepositoryPrivileges
{
    @JsonProperty("repositoryId")
    private String repositoryId;

    private Set<Privileges> repositoryPrivileges = new LinkedHashSet<>();

    private Set<PathPrivilegesDto> pathPrivileges = new LinkedHashSet<>();

    public RepositoryPrivilegesDto()
    {
    }

    @JsonCreator
    public RepositoryPrivilegesDto(@JsonProperty(value = "repositoryId", required = true) String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(final String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public Set<Privileges> getRepositoryPrivileges()
    {
        return repositoryPrivileges;
    }

    public Set<PathPrivilegesDto> getPathPrivileges()
    {
        return pathPrivileges;
    }

    public Optional<PathPrivilegesDto> getPathPrivilege(final String path,
                                                            final boolean wildcard)
    {
        return pathPrivileges.stream()
                             .filter(p -> p.getPath().equals(path) && (p.isWildcard() == wildcard))
                             .findFirst();
    }
}
