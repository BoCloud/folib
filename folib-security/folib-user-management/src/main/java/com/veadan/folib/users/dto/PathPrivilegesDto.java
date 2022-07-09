package com.veadan.folib.users.dto;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

import com.veadan.folib.users.domain.Privileges;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author 
 * @author veadan
 * @author Veadan
 */
public class PathPrivilegesDto
        implements Serializable, PathPrivileges
{

    private String path;

    private boolean wildcard;

    private Set<Privileges> privileges = new LinkedHashSet<>();

    public PathPrivilegesDto()
    {
    }

    @JsonCreator
    public PathPrivilegesDto(@JsonProperty(value = "path", required = true) String path)
    {
        this.path = path;
    }

    public String getPath()
    {
        return path;
    }

    public void setPath(final String path)
    {
        this.path = path;
    }

    public boolean isWildcard()
    {
        return wildcard;
    }

    public void setWildcard(final boolean wildcard)
    {
        this.wildcard = wildcard;
    }

    public Set<Privileges> getPrivileges()
    {
        return privileges;
    }

    public void setPrivileges(Set<Privileges> privileges)
    {
        this.privileges = privileges;
    }
}
