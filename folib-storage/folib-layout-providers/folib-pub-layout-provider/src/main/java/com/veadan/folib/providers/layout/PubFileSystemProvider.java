package com.veadan.folib.providers.layout;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 *
 */
public class PubFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(PubFileSystemProvider.class);

    @Inject
    private PubLayoutProvider layoutProvider;

    public PubFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
