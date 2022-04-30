package com.veadan.folib.providers;

/**
 * @author carlspring
 */
public class ProviderImplementationException extends Exception
{

    public ProviderImplementationException()
    {
    }

    public ProviderImplementationException(String message)
    {
        super(message);
    }

    public ProviderImplementationException(String message, Throwable cause)
    {
        super(message, cause);
    }

    public ProviderImplementationException(Throwable cause)
    {
        super(cause);
    }

}
