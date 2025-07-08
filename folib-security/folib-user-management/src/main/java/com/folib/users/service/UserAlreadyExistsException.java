package com.folib.users.service;

import org.springframework.security.authentication.InternalAuthenticationServiceException;

/**
 * @author veadan
 */
public class UserAlreadyExistsException extends InternalAuthenticationServiceException
{

    public UserAlreadyExistsException(String message,
                                      Throwable cause)
    {
        super(message, cause);
    }

    public UserAlreadyExistsException(String message)
    {
        super(message);
    }

}
