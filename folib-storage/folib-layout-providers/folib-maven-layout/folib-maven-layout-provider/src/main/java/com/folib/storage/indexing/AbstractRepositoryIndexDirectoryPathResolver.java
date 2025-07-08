package com.folib.storage.indexing;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;

/**
 * @author veadan
 */
public abstract class AbstractRepositoryIndexDirectoryPathResolver
        implements RepositoryIndexDirectoryPathResolver
{

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public RepositoryPath resolve(Repository repository)
    {
        final RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        return rootRepositoryPath.resolve(MavenRepositoryFeatures.INDEX).resolve(getIndexType().getType());
    }

    protected abstract IndexTypeEnum getIndexType();

}
