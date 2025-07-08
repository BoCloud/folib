package com.folib.storage.repository;

import java.io.Serializable;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableHttpConnectionPool
        implements Serializable
{

    private int allocatedConnections;

    public int getAllocatedConnections()
    {
        return allocatedConnections;
    }

    public void setAllocatedConnections(int allocatedConnections)
    {
        this.allocatedConnections = allocatedConnections;
    }

}
