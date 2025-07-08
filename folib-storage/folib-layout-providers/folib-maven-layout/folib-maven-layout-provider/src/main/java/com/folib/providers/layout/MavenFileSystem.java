package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.storage.indexing.RepositoryIndexCreator;
import com.folib.storage.indexing.RepositoryIndexCreator.RepositoryIndexCreatorQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author veadan
 */
public class MavenFileSystem
        extends LayoutFileSystem
{

    @Inject
    private Maven2LayoutProvider layoutProvider;

    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.HOSTED)
    private RepositoryIndexCreator hostedRepositoryIndexCreator;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.PROXY)
    private RepositoryIndexCreator proxyRepositoryIndexCreator;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.GROUP)
    private RepositoryIndexCreator groupRepositoryIndexCreator;

    public MavenFileSystem(PropertiesBooter propertiesBooter,
                           Repository repository,
                           FileSystem storageFileSystem,
                           LayoutFileSystemProvider provider)
    {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return layoutProvider.getDigestAlgorithmSet();
    }

    public RepositoryPath rebuildIndex(Repository repository)
            throws IOException
    {
        if (!mavenRepositoryFeatures.isIndexingEnabled(repository))
        {
            throw new IndexingDisabledException();
        }
        if (repository.isHostedRepository())
        {
            return hostedRepositoryIndexCreator.apply(repository);
        }
        if (repository.isGroupRepository())
        {
            return groupRepositoryIndexCreator.apply(repository);
        }
        if (repository.isProxyRepository())
        {
            return proxyRepositoryIndexCreator.apply(repository);
        }
        throw new IllegalArgumentException("Repository type not recognized. Index cannot be rebuilt.");
    }

}
