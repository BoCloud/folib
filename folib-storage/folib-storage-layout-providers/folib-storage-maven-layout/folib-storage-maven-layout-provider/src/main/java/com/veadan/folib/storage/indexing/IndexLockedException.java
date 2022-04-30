package com.veadan.folib.storage.indexing;

/**
 * @author Przemyslaw Fusik
 */
public class IndexLockedException
        extends RuntimeException
{

    public IndexLockedException(String message)
    {
        super(message);
    }
}
