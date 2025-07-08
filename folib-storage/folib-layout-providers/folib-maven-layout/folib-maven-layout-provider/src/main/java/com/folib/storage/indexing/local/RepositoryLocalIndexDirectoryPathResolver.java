package com.folib.storage.indexing.local;

import com.folib.storage.indexing.AbstractRepositoryIndexDirectoryPathResolver;
import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.LOCAL)
public class RepositoryLocalIndexDirectoryPathResolver
        extends AbstractRepositoryIndexDirectoryPathResolver
{

    @Override
    protected IndexTypeEnum getIndexType()
    {
        return IndexTypeEnum.LOCAL;
    }
}
