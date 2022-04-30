package com.veadan.folib.providers.io;

import org.apache.commons.lang3.StringUtils;

/**
 * @author Przemyslaw Fusik
 */
public abstract class RepositoryPathConstructionException
        extends RuntimeException
{

    public RepositoryPathConstructionException()
    {
        this(StringUtils.EMPTY);
    }

    public RepositoryPathConstructionException(final String message)
    {
        super(message);
    }

    public RepositoryPathConstructionException(final String message,
                                               final Throwable cause)
    {
        super(message, cause);
    }

    public RepositoryPathConstructionException(final Throwable cause)
    {
        super(cause);
    }

    public RepositoryPathConstructionException(final String message,
                                               final Throwable cause,
                                               final boolean enableSuppression,
                                               final boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
