package com.veadan.folib.users.service;

import org.springframework.security.authentication.InternalAuthenticationServiceException;

/**
 * @author sbespalov
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
