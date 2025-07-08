package com.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;
public class HelmFileSystemProvider extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(HelmFileSystemProvider.class);

    @Inject
    private HelmLayoutProvider layoutProvider;

    public HelmFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

}