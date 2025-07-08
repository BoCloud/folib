package com.folib.storage.indexing.remote;

import com.folib.storage.indexing.AbstractRepositoryIndexDirectoryPathResolver;
import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;

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
