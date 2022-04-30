package com.veadan.folib.providers.storage;

import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author carlspring
 */
public interface StorageProvider
{

    String getAlias();

    void register();

    FileSystem getFileSystem();
    
    FileSystemProvider getFileSystemProvider();

}
