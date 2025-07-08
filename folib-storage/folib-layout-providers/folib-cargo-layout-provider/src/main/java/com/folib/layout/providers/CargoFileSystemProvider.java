package com.folib.layout.providers;

import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

public class CargoFileSystemProvider extends LayoutFileSystemProvider {

    @Inject
    private CargoLayoutProvider cargoLayoutProvider;

    public CargoFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return cargoLayoutProvider;
    }
}
