package com.veadan.folib.repository;

/**
 * @author carlspring
 */
public class RepositoryManagementStrategyException extends Exception
{

    public RepositoryManagementStrategyException()
    {
    }

    public RepositoryManagementStrategyException(String message)
    {
        super(message);
    }

    public RepositoryManagementStrategyException(String message,
                                                 Throwable cause)
    {
        super(message, cause);
    }

    public RepositoryManagementStrategyException(Throwable cause)
    {
        super(cause);
    }

    public RepositoryManagementStrategyException(String message,
                                                 Throwable cause,
                                                 boolean enableSuppression,
                                                 boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
