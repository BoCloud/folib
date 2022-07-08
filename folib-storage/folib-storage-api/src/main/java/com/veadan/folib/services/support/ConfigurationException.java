package com.veadan.folib.services.support;

/**
 * @author veadan
 */
public abstract class ConfigurationException
        extends RuntimeException
{

    public ConfigurationException(final Throwable cause)
    {
        super(cause);
    }

    public ConfigurationException(final String message)
    {
        super(message);
    }
}
