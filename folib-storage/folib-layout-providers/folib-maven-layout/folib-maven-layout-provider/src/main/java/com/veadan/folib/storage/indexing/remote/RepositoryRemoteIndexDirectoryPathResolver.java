package com.veadan.folib.storage.indexing.remote;

import com.veadan.folib.storage.indexing.AbstractRepositoryIndexDirectoryPathResolver;
import com.veadan.folib.storage.indexing.IndexTypeEnum;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
public class RepositoryRemoteIndexDirectoryPathResolver
        extends AbstractRepositoryIndexDirectoryPathResolver
{

    @Override
    protected IndexTypeEnum getIndexType()
    {
        return IndexTypeEnum.REMOTE;
    }

}
