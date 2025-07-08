package com.folib.providers.layout;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author veadan
 * @date 2023/8/2 15:08
 */
public class CocoapodsFileSystemProvider extends LayoutFileSystemProvider
{
    @Inject
    private CocoapodsLayoutProvider cocoapodsLayoutProvider;
    
    
    public CocoapodsFileSystemProvider(FileSystemProvider storageFileSystemProvider) 
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return cocoapodsLayoutProvider;
    }
}
