package com.folib.providers;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 *
 */
public class RawFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(RawFileSystemProvider.class);

    @Inject
    private RawLayoutProvider layoutProvider;

    public RawFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
