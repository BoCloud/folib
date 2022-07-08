package com.veadan.folib.storage.indexing;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;

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
