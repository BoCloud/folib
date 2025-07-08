package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author veadan
 * @since 2024-08-27 17:16
 */
public class DebianFileSystem extends LayoutFileSystem {

    @Inject
    private DebianLayoutProvider layoutProvider;


    public DebianFileSystem(PropertiesBooter propertiesBooter,
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
