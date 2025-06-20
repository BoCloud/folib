package com.veadan.folib.providers.layout;

import java.nio.file.FileSystem;
import java.util.Set;

import javax.inject.Inject;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author xuxinping
 *
 */
public class NpmFileSystem extends LayoutFileSystem
{

    @Inject
    private NpmLayoutProvider layoutProvider;

    public NpmFileSystem(PropertiesBooter propertiesBooter,
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
