package com.folib.storage.indexing;

/**
 * @author veadan
 */
public class IndexLockedException
        extends RuntimeException
{

    public IndexLockedException(String message)
    {
        super(message);
    }
}
