package com.veadan.folib.storage.indexing;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.util.ThrowingFunction;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

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
