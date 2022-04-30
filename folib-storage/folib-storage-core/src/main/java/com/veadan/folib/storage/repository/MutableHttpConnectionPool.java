package com.veadan.folib.storage.repository;

import java.io.Serializable;

/**
 * @author korest
 * @author Pablo Tirado
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
