package com.folib.users.domain;

import static java.util.stream.Collectors.toSet;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import javax.annotation.concurrent.Immutable;

import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.RepositoryPrivileges;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class RepositoryPrivilegesData
        implements Serializable, RepositoryPrivileges
{

    private final String repositoryId;

    private final Set<Privileges> repositoryPrivileges;

    private final Set<PathPrivilegesData> pathPrivileges;

    public RepositoryPrivilegesData(final RepositoryPrivilegesDto delegate)
    {
        this.repositoryId = delegate.getRepositoryId();
        this.repositoryPrivileges = immuteRepositoryPrivileges(delegate.getRepositoryPrivileges());
        this.pathPrivileges = immutePathPrivileges(delegate.getPathPrivileges());
    }

    private Set<Privileges> immuteRepositoryPrivileges(final Set<Privileges> set)
    {
        return set != null ? ImmutableSet.copyOf(set)
                : Collections.emptySet();
    }

    private Set<PathPrivilegesData> immutePathPrivileges(final Set<PathPrivilegesDto> source)
    {
        return source != null ?
               ImmutableSet.copyOf(source.stream().map(PathPrivilegesData::new).collect(toSet())) :
               Collections.emptySet();
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public Set<Privileges> getRepositoryPrivileges()
    {
        return repositoryPrivileges;
    }

    public Set<PathPrivilegesData> getPathPrivileges()
    {
        return pathPrivileges;
    }
}
