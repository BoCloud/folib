package com.veadan.folib.providers.layout;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author xuxinping
 *
 */
public class NpmFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(NpmFileSystemProvider.class);

    @Inject
    private NpmLayoutProvider layoutProvider;

    public NpmFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
