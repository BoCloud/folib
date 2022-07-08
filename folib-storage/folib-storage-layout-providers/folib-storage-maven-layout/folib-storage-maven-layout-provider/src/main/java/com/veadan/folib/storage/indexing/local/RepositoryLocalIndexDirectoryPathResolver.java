package com.veadan.folib.storage.indexing.local;

import com.veadan.folib.storage.indexing.AbstractRepositoryIndexDirectoryPathResolver;
import com.veadan.folib.storage.indexing.IndexTypeEnum;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;

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
