package com.folib.providers.layout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

public class CjpmFileSystemProvider extends LayoutFileSystemProvider
{

    @Inject
    private CjpmLayoutProvider layoutProvider;

    public CjpmFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
