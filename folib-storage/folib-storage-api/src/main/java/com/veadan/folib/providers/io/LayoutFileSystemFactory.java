package com.veadan.folib.providers.io;

import com.veadan.folib.storage.repository.Repository;

@FunctionalInterface
public interface LayoutFileSystemFactory
{

    LayoutFileSystem create(Repository repository);
    
}
