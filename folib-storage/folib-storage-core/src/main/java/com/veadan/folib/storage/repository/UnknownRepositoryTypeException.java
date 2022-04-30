package com.veadan.folib.storage.repository;

/**
 * @author carlspring
 */
public class UnknownRepositoryTypeException
        extends Exception
{

    public UnknownRepositoryTypeException()
    {
    }

    public UnknownRepositoryTypeException(String message)
    {
        super(message);
    }

    public UnknownRepositoryTypeException(String message,
                                          Throwable cause)
    {
        super(message, cause);
    }

    public UnknownRepositoryTypeException(Throwable cause)
    {
        super(cause);
    }

    public UnknownRepositoryTypeException(String message,
                                          Throwable cause,
                                          boolean enableSuppression,
                                          boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
