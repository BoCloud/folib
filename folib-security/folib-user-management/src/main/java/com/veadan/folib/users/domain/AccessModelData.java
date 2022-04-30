package com.veadan.folib.users.domain;

import static java.util.stream.Collectors.toSet;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.concurrent.Immutable;

import com.veadan.folib.users.dto.PathPrivileges;
import com.veadan.folib.users.dto.StoragePrivilegesDto;
import org.apache.commons.lang.StringUtils;
import com.veadan.folib.users.dto.AccessModel;
import com.veadan.folib.users.dto.AccessModelDto;
import com.veadan.folib.users.dto.RepositoryPrivileges;
import com.veadan.folib.users.dto.StoragePrivileges;

import com.google.common.collect.ImmutableSet;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class AccessModelData
        implements Serializable, AccessModel
{

    private final Set<Privileges> apiAuthorities;
    
    private final Set<StoragePrivilegesData> storageAuthorities;


    public AccessModelData(AccessModelDto delegate)
    {
        this.storageAuthorities = immuteStorages(delegate.getStorageAuthorities());
        this.apiAuthorities = ImmutableSet.copyOf(delegate.getApiAuthorities());
    }

    private Set<StoragePrivilegesData> immuteStorages(final Set<StoragePrivilegesDto> source)
    {
        return source != null ?
               ImmutableSet.copyOf(
                       source.stream().map(StoragePrivilegesData::new).collect(toSet())) :
               Collections.emptySet();
    }

    @Override
    public Set<Privileges> getApiAuthorities()
    {
        return apiAuthorities;
    }

    public Set<StoragePrivilegesData> getStorageAuthorities()
    {
        return storageAuthorities;
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return getPathAuthorities(url, getStorageAuthorities());
    }
    
    public static Set<Privileges> getPathAuthorities(String url, Set<? extends StoragePrivileges> storages)
    {
        String normalizedUrl = StringUtils.chomp(url, "/");

        Set<Privileges> privileges = new HashSet<>();
        for (final StoragePrivileges storage : storages)
        {
            String storageKey = "/storages/" + storage.getStorageId();
            if (!normalizedUrl.startsWith(storageKey))
            {
                continue;
            }
            for (RepositoryPrivileges repository : storage.getRepositoryPrivileges())
            {
                String repositoryKey = storageKey + "/" + repository.getRepositoryId();
                if (!normalizedUrl.startsWith(repositoryKey))
                {
                    continue;
                }
                privileges.addAll(repository.getRepositoryPrivileges());
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges())
                {
                    String normalizedPath = StringUtils.chomp(pathPrivilege.getPath(), "/");
                    String pathKey = repositoryKey + "/" + normalizedPath;

                    if (!normalizedUrl.startsWith(pathKey))
                    {
                        continue;
                    }
                    if (normalizedUrl.equals(pathKey) || pathPrivilege.isWildcard())
                    {
                        privileges.addAll(pathPrivilege.getPrivileges());
                    }
                }
            }
        }
        return privileges;
    }
    
}
