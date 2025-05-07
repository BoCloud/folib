package com.veadan.folib.storage.repository;

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
public class HttpConnectionPool
{

    private int allocatedConnections;

    HttpConnectionPool()
    {

    }


    public HttpConnectionPool(final MutableHttpConnectionPool delegate)
    {
        allocatedConnections = delegate.getAllocatedConnections();
    }

    public int getAllocatedConnections()
    {
        return allocatedConnections;
    }
}
