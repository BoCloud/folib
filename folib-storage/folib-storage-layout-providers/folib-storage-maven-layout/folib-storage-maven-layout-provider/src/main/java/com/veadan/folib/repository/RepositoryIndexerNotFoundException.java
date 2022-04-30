package com.veadan.folib.repository;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryIndexerNotFoundException
        extends RuntimeException
{

    public RepositoryIndexerNotFoundException(final String message)
    {
        super(message);
    }
}
