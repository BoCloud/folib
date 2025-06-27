package com.veadan.folib.providers.layout;

import lombok.extern.slf4j.Slf4j;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author veadan
 * @since 2024-08-27 17:16
 */
@Slf4j
public class DebianFileSystemProvider extends LayoutFileSystemProvider {

    @Inject
    private DebianLayoutProvider layoutProvider;

    public DebianFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

}
