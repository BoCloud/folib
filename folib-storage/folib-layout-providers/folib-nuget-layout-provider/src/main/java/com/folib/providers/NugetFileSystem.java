package com.folib.providers;

import java.nio.file.FileSystem;
import java.util.Set;

import javax.inject.Inject;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 *
 */
public class NugetFileSystem extends LayoutFileSystem
{

    @Inject
    private NugetLayoutProvider layoutProvider;

    public NugetFileSystem(PropertiesBooter propertiesBooter,
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
