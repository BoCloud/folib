package com.folib.exception;

/**
 * @author Veadan
 */
public class ServiceUnavailableException
        extends RuntimeException
{

    public ServiceUnavailableException(final String message)
    {
        super(message);
    }

}
