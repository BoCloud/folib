package com.veadan.folib.event;

import java.nio.file.Path;

/**
 * @author Veadan
 */
public class RepositoryBasedEvent<T extends Path> extends Event
{

    private T path;

    public RepositoryBasedEvent(T path,
                                int type)
    {
        super(type);
        this.path = path;
    }

    public T getPath()
    {
        return path;
    }

    public void setPath(T path)
    {
        this.path = path;
    }

}
