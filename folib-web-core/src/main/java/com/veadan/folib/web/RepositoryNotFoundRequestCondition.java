package com.veadan.folib.web;


import jakarta.servlet.http.HttpServletRequest;

import java.util.Collection;
import java.util.Collections;

import static com.veadan.folib.web.Constants.REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE;

/**
 * @author veadan
 */
public class RepositoryNotFoundRequestCondition
        extends ExposableRequestCondition
{

    private final String repositoryId;

    public RepositoryNotFoundRequestCondition(String repositoryId)
    {

        this.repositoryId = repositoryId;
    }

    @Override
    protected void expose(HttpServletRequest request)
    {
        request.setAttribute(REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE, repositoryId);
    }

    @Override
    protected Collection<?> getContent()
    {
        return Collections.singletonList(repositoryId);
    }

    @Override
    protected String getToStringInfix()
    {
        return repositoryId;
    }
}
