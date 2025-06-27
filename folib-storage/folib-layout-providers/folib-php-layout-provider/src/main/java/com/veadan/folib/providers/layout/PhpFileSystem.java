package com.veadan.folib.providers.layout;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author veadan
 */
public class PhpFileSystem extends LayoutFileSystem {

    @Autowired
    private PhpLayoutProvider layoutProvider;

    public PhpFileSystem(PropertiesBooter propertiesBooter,
                         Repository repository,
                         FileSystem storageFileSystem,
                         LayoutFileSystemProvider provider) {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return layoutProvider.getDigestAlgorithmSet();
    }

}
