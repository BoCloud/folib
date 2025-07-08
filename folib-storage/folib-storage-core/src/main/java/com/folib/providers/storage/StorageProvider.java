package com.folib.providers.storage;

import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author Veadan
 */
public interface StorageProvider
{

    String getAlias();

    void register();

    FileSystem getFileSystem();
    
    FileSystemProvider getFileSystemProvider();

}
