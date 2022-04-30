package com.veadan.folib.web;

import com.veadan.folib.storage.repository.Repository;

import javax.annotation.Nonnull;
import javax.servlet.http.HttpServletRequest;
import java.util.Collection;
import java.util.Collections;

import static com.veadan.folib.web.Constants.REPOSITORY_REQUEST_ATTRIBUTE;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryRequestCondition
        extends ExposableRequestCondition
{

    private final Repository repository;

    RepositoryRequestCondition(@Nonnull final Repository repository)
    {
        this.repository = repository;
    }

    @Override
    protected Collection<?> getContent()
    {
        return Collections.singletonList(repository);
    }

    @Override
    protected String getToStringInfix()
    {
        return repository.getId();
    }

    @Nonnull
    protected Repository getRepository()
    {
        return this.repository;
    }

    @Override
    protected void expose(HttpServletRequest request)
    {
        request.setAttribute(REPOSITORY_REQUEST_ATTRIBUTE, repository);
    }
}
