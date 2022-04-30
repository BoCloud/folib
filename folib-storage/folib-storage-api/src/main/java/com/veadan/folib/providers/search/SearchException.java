package com.veadan.folib.providers.search;

/**
 * @author carlspring
 */
public class SearchException
        extends Exception
{


    public SearchException()
    {
    }

    public SearchException(String message)
    {
        super(message);
    }

    public SearchException(String message,
                           Throwable cause)
    {
        super(message, cause);
    }

    public SearchException(Throwable cause)
    {
        super(cause);
    }

    public SearchException(String message,
                           Throwable cause,
                           boolean enableSuppression,
                           boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
