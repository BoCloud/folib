package com.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.spi.FileSystemProvider;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
public class GoFileSystemProvider extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(GoFileSystemProvider.class);

    @Autowired
    private GoLayoutProvider layoutProvider;

    public GoFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }


}
