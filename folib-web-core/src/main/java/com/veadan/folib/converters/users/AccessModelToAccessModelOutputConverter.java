package com.veadan.folib.converters.users;

import com.veadan.folib.controllers.users.support.AccessModelOutput;
import com.veadan.folib.controllers.users.support.RepositoryAccessModelOutput;
import com.veadan.folib.users.domain.AccessModelData;
import com.veadan.folib.users.dto.PathPrivileges;
import com.veadan.folib.users.dto.RepositoryPrivileges;
import com.veadan.folib.users.dto.StoragePrivileges;

import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Przemyslaw Fusik
 */
public enum AccessModelToAccessModelOutputConverter
        implements Converter<AccessModelData, AccessModelOutput>
{
    INSTANCE;
    
    @Override
    public AccessModelOutput convert(final AccessModelData source)
    {
        if (source == null)
        {
            return null;
        }

        AccessModelOutput result = new AccessModelOutput();
        for (StoragePrivileges storage : source.getStorageAuthorities())
        {
            for (RepositoryPrivileges repository : storage.getRepositoryPrivileges())
            {
                if (CollectionUtils.isNotEmpty(repository.getRepositoryPrivileges()))
                {
                    RepositoryAccessModelOutput repositoryAccess = getRepositoryAccessOrAddNewOne(result,
                                                                                                  storage.getStorageId(),
                                                                                                  repository.getRepositoryId(),
                                                                                                  null,
                                                                                                  false);
                    repositoryAccess.getPrivileges()
                                    .addAll(repository.getRepositoryPrivileges()
                                                      .stream()
                                                      .map(p -> p.name())
                                                      .collect(Collectors.toSet()));
                }
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges())
                {
                    RepositoryAccessModelOutput repositoryAccess = getRepositoryAccessOrAddNewOne(result,
                                                                                                  storage.getStorageId(),
                                                                                                  repository.getRepositoryId(),
                                                                                                  pathPrivilege.getPath(),
                                                                                                  pathPrivilege.isWildcard());

                    repositoryAccess.getPrivileges().addAll(pathPrivilege.getPrivileges()
                                                                         .stream()
                                                                         .map(p -> p.name())
                                                                         .collect(Collectors.toSet()));
                }
            }
        }

        return result;
    }

    private RepositoryAccessModelOutput getRepositoryAccessOrAddNewOne(final AccessModelOutput result,
                                                                       final String storageId,
                                                                       final String repositoryId,
                                                                       final String path,
                                                                       final boolean wildcard)
    {
        return result.getRepositoryAccess(storageId,
                                          repositoryId,
                                          path,
                                          wildcard)
                     .orElseGet(() ->
                                {
                                    RepositoryAccessModelOutput repositoryAccess = new RepositoryAccessModelOutput();
                                    repositoryAccess.setRepositoryId(repositoryId);
                                    repositoryAccess.setStorageId(storageId);
                                    repositoryAccess.setPath(path);
                                    repositoryAccess.setWildcard(wildcard);
                                    result.getRepositoriesAccess().add(repositoryAccess);
                                    return repositoryAccess;
                                });
    }
}
