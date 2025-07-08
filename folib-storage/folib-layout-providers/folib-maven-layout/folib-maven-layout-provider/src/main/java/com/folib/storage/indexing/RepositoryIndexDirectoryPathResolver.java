package com.folib.storage.indexing;

import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;

import javax.inject.Qualifier;
import java.lang.annotation.Retention;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * @author veadan
 */
public interface RepositoryIndexDirectoryPathResolver
{

    RepositoryPath resolve(Repository repository);

    @Qualifier
    @Retention(RUNTIME)
    @interface RepositoryIndexDirectoryPathResolverQualifier
    {

        IndexTypeEnum value();
    }
}
