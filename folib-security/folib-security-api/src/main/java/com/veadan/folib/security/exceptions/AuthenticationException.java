package com.veadan.folib.security.exceptions;

import javax.security.auth.login.LoginException;

/**
 * @author veadan
 */
public class AuthenticationException extends LoginException
{

    public AuthenticationException()
    {
    }

    public AuthenticationException(String message)
    {
        super(message);
    }

}
