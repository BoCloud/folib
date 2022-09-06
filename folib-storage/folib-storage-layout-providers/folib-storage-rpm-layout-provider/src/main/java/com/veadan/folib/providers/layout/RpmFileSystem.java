package com.veadan.folib.providers.layout;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

public class RpmFileSystem  extends LayoutFileSystem
{

    @Inject
    private RpmLayoutProvider layoutProvider;

    public RpmFileSystem(PropertiesBooter propertiesBooter,
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
