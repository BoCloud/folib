package com.veadan.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;


public class RpmFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(RpmFileSystemProvider.class);

    @Inject
    private RpmLayoutProvider layoutProvider;

    public RpmFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
