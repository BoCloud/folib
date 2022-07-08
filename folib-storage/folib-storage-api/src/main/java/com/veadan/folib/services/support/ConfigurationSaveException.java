package com.veadan.folib.services.support;

/**
 * @author veadan
 */
public class ConfigurationSaveException
        extends ConfigurationException
{

    public ConfigurationSaveException(final Throwable cause)
    {
        super(cause);
    }

    public ConfigurationSaveException(final String message)
    {
        super(message);
    }
}
