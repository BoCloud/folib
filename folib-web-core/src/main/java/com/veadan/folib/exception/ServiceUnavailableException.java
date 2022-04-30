package com.veadan.folib.exception;

/**
 * @author Pablo Tirado
 */
public class ServiceUnavailableException
        extends RuntimeException
{

    public ServiceUnavailableException(final String message)
    {
        super(message);
    }

}
