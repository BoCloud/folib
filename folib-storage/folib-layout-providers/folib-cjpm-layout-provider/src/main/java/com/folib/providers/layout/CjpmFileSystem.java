package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.storage.repository.Repository;
import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

public class CjpmFileSystem extends LayoutFileSystem
{

    @Inject
    private CjpmLayoutProvider layoutProvider;

    public CjpmFileSystem(PropertiesBooter propertiesBooter,
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
