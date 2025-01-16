package com.veadan.folib.layout.providers;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.util.Set;

public class CargoFileSystem extends LayoutFileSystem {

    @Inject
    private CargoLayoutProvider cargoLayoutProvider;

    public CargoFileSystem(PropertiesBooter propertiesBooter,
                           Repository repository,
                           FileSystem storageFileSystem,
                           LayoutFileSystemProvider provider) {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return cargoLayoutProvider.getDigestAlgorithmSet();
    }
}
