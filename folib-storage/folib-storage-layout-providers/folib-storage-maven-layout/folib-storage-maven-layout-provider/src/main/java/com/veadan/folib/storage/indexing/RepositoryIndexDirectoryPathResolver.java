package com.veadan.folib.storage.indexing;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;

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
