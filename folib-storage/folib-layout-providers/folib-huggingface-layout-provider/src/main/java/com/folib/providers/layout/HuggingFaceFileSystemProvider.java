package com.folib.providers.layout;

import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;

public class HuggingFaceFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(HuggingFaceFileSystemProvider.class);

    @Inject
    private HuggingFaceLayoutProvider layoutProvider;

    public HuggingFaceFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
