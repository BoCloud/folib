package com.folib.storage.indexing;

import com.folib.providers.io.RepositoryPath;
import com.folib.util.ThrowingFunction;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Qualifier;
import java.lang.annotation.Retention;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.io.IOException;

/**
 * @author veadan
 */
public interface RepositoryIndexCreator
        extends ThrowingFunction<Repository, RepositoryPath>
{

    @Override
    RepositoryPath apply(Repository t) throws IOException;

    @Qualifier
    @Retention(RUNTIME)
    @interface RepositoryIndexCreatorQualifier
    {

        RepositoryTypeEnum value();
    }
}
