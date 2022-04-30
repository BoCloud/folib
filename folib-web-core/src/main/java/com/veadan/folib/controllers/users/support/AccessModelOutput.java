package com.veadan.folib.controllers.users.support;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @author Przemyslaw Fusik
 */
public class AccessModelOutput
{

    private List<RepositoryAccessModelOutput> repositoriesAccess = new ArrayList<>();

    public List<RepositoryAccessModelOutput> getRepositoriesAccess()
    {
        return repositoriesAccess;
    }

    public void setRepositoriesAccess(final List<RepositoryAccessModelOutput> repositoriesAccess)
    {
        this.repositoriesAccess = repositoriesAccess;
    }

    public Optional<RepositoryAccessModelOutput> getRepositoryAccess(final String storageId,
                                                                     final String repositoryId,
                                                                     final String path,
                                                                     final boolean wildcard)
    {
        return repositoriesAccess.stream().filter(ra -> Objects.equals(ra.getStorageId(), storageId) &&
                                                        Objects.equals(ra.getRepositoryId(), repositoryId) &&
                                                        Objects.equals(ra.getPath(), path) &&
                                                        ra.isWildcard() == wildcard).findFirst();
    }
}
