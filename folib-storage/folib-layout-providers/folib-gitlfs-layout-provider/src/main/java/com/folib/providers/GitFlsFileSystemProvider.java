package com.folib.providers;

import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.nio.file.spi.FileSystemProvider;


public class GitFlsFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(GitFlsFileSystemProvider.class);

    @Inject
    private GitLfsLayoutProvider layoutProvider;

    public GitFlsFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

}
