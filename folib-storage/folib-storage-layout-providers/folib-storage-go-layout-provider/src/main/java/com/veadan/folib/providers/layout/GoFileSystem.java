package com.veadan.folib.providers.layout;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
public class GoFileSystem extends LayoutFileSystem {

    @Autowired
    private GoLayoutProvider layoutProvider;

    public GoFileSystem(PropertiesBooter propertiesBooter,
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
