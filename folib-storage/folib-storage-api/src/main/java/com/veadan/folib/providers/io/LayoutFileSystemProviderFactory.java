package com.veadan.folib.providers.io;

import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author xuxinping
 *
 */
@FunctionalInterface
public interface LayoutFileSystemProviderFactory
{

    LayoutFileSystemProvider create(Repository repository);

}
