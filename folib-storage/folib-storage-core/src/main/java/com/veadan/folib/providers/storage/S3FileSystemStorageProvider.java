package com.veadan.folib.providers.storage;

import com.veadan.folib.cloud.storage.s3fs.S3FileSystemProvider;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;

/**
 * @author Veadan
 */
@Slf4j
@Component("s3filesystemStorageProvider")
public class S3FileSystemStorageProvider
        extends AbstractStorageProvider {

    private static final Logger logger = LoggerFactory.getLogger(S3FileSystemStorageProvider.class);

    public static final String ALIAS = "s3";

    @Inject
    private FileSystem s3FileSystem;

    @Inject
    private S3FileSystemProvider s3FileSystemProvider;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @PostConstruct
    @Override
    public void register() {
        logger.info("Registered storage provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public FileSystem getFileSystem() {
        return s3FileSystem;
    }

    @Override
    public FileSystemProvider getFileSystemProvider() {
        List<FileSystemProvider> installedProviders = FileSystemProvider.installedProviders();
        for (FileSystemProvider fileSystemProvider : installedProviders) {
            boolean a = "S3FileSystemProvider".equalsIgnoreCase(fileSystemProvider.getClass().getSimpleName());
            if (a) {
                return fileSystemProvider;
            }

        }
        return s3FileSystemProvider;
    }

}
