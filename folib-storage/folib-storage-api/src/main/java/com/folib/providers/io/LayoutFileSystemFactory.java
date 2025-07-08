package com.folib.providers.io;

import com.folib.storage.repository.Repository;

@FunctionalInterface
public interface LayoutFileSystemFactory
{

    LayoutFileSystem create(Repository repository);
    
}
