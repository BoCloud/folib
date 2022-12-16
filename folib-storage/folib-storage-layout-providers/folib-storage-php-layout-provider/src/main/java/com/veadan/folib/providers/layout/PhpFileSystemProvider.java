package com.veadan.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.spi.FileSystemProvider;

/**
 * @author leipenghui
 */
public class PhpFileSystemProvider extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(PhpFileSystemProvider.class);

    @Autowired
    private PhpLayoutProvider layoutProvider;

    public PhpFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

}
