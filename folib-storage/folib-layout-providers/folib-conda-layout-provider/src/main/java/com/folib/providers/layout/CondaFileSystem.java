package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.FileSystem;
import java.util.HashSet;
import java.util.Set;

public class CondaFileSystem extends LayoutFileSystem {

    @Autowired
    private CondaLayoutProvider layoutProvider;

    public CondaFileSystem(PropertiesBooter propertiesBooter,
                           Repository repository,
                           FileSystem storageFileSystem,
                           LayoutFileSystemProvider provider) {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
//        return layoutProvider.getDigestAlgorithmSet();
        return new HashSet<>();
    }
}
