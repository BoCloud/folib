package com.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

public class ConanFileSystemProvider extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(ConanFileSystemProvider.class);

    @Inject
    private ConanLayoutProvider layoutProvider;

    public ConanFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }
}