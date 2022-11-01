package com.veadan.folib.providers.storage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;

/**
 * @author Veadan
 */
@Component("s3filesystemStorageProvider")
public class S3FileSystemStorageProvider
        extends AbstractStorageProvider
{

    private static final Logger logger = LoggerFactory.getLogger(S3FileSystemStorageProvider.class);

    public static final String ALIAS = "s3";

    @Inject
    private FileSystem s3FileSystem;
    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @PostConstruct
    @Override
    public void register()
    {
        logger.info("Registered storage provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public FileSystem getFileSystem()
    {

        return s3FileSystem;

    }

    @Override
    public FileSystemProvider getFileSystemProvider()
    {
        List<FileSystemProvider> installedProviders = FileSystemProvider.installedProviders();
        for (FileSystemProvider fileSystemProvider : installedProviders)
        {
            boolean a=fileSystemProvider.getClass().getSimpleName().equals("S3FileSystemProvider");
//            if (!"file".equals(fileSystemProvider.getScheme()))
//            {
//                continue;
//            }
            if(a){
                return fileSystemProvider;
            }

        }

        return null;
    }

}
