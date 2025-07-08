package com.folib.users.domain;

import static java.util.stream.Collectors.toSet;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import javax.annotation.concurrent.Immutable;

import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.StoragePrivileges;
import com.folib.users.dto.StoragePrivilegesDto;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class StoragePrivilegesData
        implements Serializable, StoragePrivileges
{

    private final Set<RepositoryPrivilegesData> repositories;

    private final Set<Privileges> storagePrivileges;

    private final String storageId;

    public StoragePrivilegesData(final StoragePrivilegesDto delegate)
    {
        this.repositories = immuteRepositories(delegate.getRepositoryPrivileges());
        this.storageId = delegate.getStorageId();
        this.storagePrivileges = immuteStorage(delegate.getStoragePrivileges());
    }

    private Set<RepositoryPrivilegesData> immuteRepositories(final Set<RepositoryPrivilegesDto> source)
    {
        return source != null ?
               ImmutableSet.copyOf(source.stream().map(RepositoryPrivilegesData::new).collect(toSet())) :
               Collections.emptySet();
    }

    private Set<Privileges> immuteStorage(final Set<Privileges> set)
    {
        return set != null ? ImmutableSet.copyOf(set)
                : Collections.emptySet();
    }

    @Override
    public Set<RepositoryPrivilegesData> getRepositoryPrivileges()
    {
        return repositories;
    }

    @Override
    public Set<Privileges> getStoragePrivileges() {
        return storagePrivileges;
    }

    @Override
    public String getStorageId()
    {
        return storageId;
    }
}
