package com.veadan.folib.exception;

/**
 * @author Pablo Tirado
 */
public class StorageNotFoundException
        extends RuntimeException
{

    public StorageNotFoundException(final String message)
    {
        super(message);
    }

}
