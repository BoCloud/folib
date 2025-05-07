package com.veadan.folib.configuration;

import javax.annotation.concurrent.Immutable;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class SessionConfiguration
{

    private Integer timeoutSeconds;

    SessionConfiguration()
    {

    }


    public SessionConfiguration(final MutableSessionConfiguration delegate)
    {
        this.timeoutSeconds = delegate.getTimeoutSeconds();
    }

    public Integer getTimeoutSeconds()
    {
        return timeoutSeconds;
    }
}
