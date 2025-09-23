package com.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.spi.FileSystemProvider;


public class CondaFileSystemProvider extends LayoutFileSystemProvider{
    private static final Logger logger = LoggerFactory.getLogger(CondaFileSystemProvider.class);

    @Autowired
    private CondaLayoutProvider layoutProvider;

    public CondaFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

}
