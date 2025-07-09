package com.folib.providers;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Veadan
 */
public class PypiFileSystemProvider
        extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(PypiFileSystemProvider.class);

    @Inject
    private PypiLayoutProvider layoutProvider;

    public PypiFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
