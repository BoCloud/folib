package com.folib.exception;

/**
 * @author Veadan
 */
public class StorageNotFoundException
        extends RuntimeException
{

    public StorageNotFoundException(final String message)
    {
        super(message);
    }

}
