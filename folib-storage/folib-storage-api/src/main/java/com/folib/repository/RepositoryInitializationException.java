package com.folib.repository;

import java.io.IOException;

/**
 * @author Veadan
 */
public class RepositoryInitializationException
        extends IOException
{

    public RepositoryInitializationException()
    {
    }

    public RepositoryInitializationException(String message)
    {
        super(message);
    }

    public RepositoryInitializationException(String message,
                                             Throwable cause)
    {
        super(message, cause);
    }

    public RepositoryInitializationException(Throwable cause)
    {
        super(cause);
    }

}
