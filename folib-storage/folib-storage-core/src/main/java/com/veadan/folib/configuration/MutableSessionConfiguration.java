package com.veadan.folib.configuration;

import java.io.Serializable;

/**
 * @author Przemyslaw Fusik
 * @author Pablo Tirado
 */
public class MutableSessionConfiguration
        implements Serializable
{

    private Integer timeoutSeconds = 3600;

    public Integer getTimeoutSeconds()
    {
        return timeoutSeconds;
    }

    public void setTimeoutSeconds(Integer timeoutSeconds)
    {
        this.timeoutSeconds = timeoutSeconds;
    }
}
