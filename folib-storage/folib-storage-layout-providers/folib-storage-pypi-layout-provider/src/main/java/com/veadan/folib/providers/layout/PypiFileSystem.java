package com.veadan.folib.providers.layout;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author carlspring
 */
public class PypiFileSystem
        extends LayoutFileSystem
{

    @Inject
    private PypiLayoutProvider layoutProvider;

    public PypiFileSystem(PropertiesBooter propertiesBooter,
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

}
