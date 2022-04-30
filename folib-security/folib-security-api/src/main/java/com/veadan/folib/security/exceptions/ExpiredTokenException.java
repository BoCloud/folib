package com.veadan.folib.security.exceptions;

/**
 * @author Przemyslaw Fusik
 */
public class ExpiredTokenException extends InvalidTokenException
{

    public ExpiredTokenException(String msg,
                                         Throwable t)
    {
        super(msg, t);
    }

    public ExpiredTokenException(String msg)
    {
        super(msg);
    }
}
