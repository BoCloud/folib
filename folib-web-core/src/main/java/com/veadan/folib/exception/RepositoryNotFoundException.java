package com.veadan.folib.exception;

/**
 * @author Pablo Tirado
 */
public class RepositoryNotFoundException
        extends RuntimeException
{

    public RepositoryNotFoundException(final String message)
    {
        super(message);
    }

}
