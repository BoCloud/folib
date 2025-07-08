package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author veadan
 * @date 2023/8/2 15:03
 */
public class CocoapodsFileSystem extends LayoutFileSystem 
{
    @Inject
    private CocoapodsLayoutProvider layoutProvider;
    
    public CocoapodsFileSystem(PropertiesBooter propertiesBooter, Repository repository, FileSystem storageFileSystem, LayoutFileSystemProvider provider)
    {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return layoutProvider.getDigestAlgorithmSet();
    }
}
