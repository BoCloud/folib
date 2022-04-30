package com.veadan.folib.security.exceptions;

/**
 * @author mtodorov
 */
public class UserResolutionException extends Exception
{

    public UserResolutionException()
    {
    }

    public UserResolutionException(String message)
    {
        super(message);
    }

    public UserResolutionException(String message,
                                   Throwable cause)
    {
        super(message, cause);
    }

    public UserResolutionException(Throwable cause)
    {
        super(cause);
    }

}
