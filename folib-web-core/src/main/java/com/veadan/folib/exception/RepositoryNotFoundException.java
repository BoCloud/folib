package com.veadan.folib.exception;

/**
 * @author Veadan
 */
public class RepositoryNotFoundException
        extends RuntimeException
{

    public RepositoryNotFoundException(final String message)
    {
        super(message);
    }

}
