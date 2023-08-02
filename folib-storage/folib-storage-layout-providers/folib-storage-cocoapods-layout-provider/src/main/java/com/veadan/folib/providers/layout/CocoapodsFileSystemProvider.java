package com.veadan.folib.providers.layout;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 15:08
 * @since x.x.x
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
