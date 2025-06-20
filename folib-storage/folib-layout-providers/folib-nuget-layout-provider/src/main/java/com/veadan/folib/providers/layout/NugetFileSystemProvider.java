package com.veadan.folib.providers.layout;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.io.LayoutOutputStream;

import javax.inject.Inject;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.spi.FileSystemProvider;
import java.security.NoSuchAlgorithmException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author xuxinping
 *
 */
public class NugetFileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(NugetFileSystemProvider.class);

    @Inject
    private NugetLayoutProvider layoutProvider;

    public NugetFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

    @Override
    protected LayoutOutputStream decorateStream(RepositoryPath path,
                                                OutputStream os)
        throws NoSuchAlgorithmException,
        IOException
    {
        LayoutOutputStream result = super.decorateStream(path, os);
        result.setDigestStringifier(layoutProvider::toBase64);
        return result;
    }
}
