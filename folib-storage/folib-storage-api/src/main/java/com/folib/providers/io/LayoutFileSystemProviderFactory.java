package com.folib.providers.io;

import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 *
 */
@FunctionalInterface
public interface LayoutFileSystemProviderFactory
{

    LayoutFileSystemProvider create(Repository repository);

}
