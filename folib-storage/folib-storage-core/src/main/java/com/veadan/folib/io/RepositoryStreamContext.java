package com.veadan.folib.io;

import java.nio.file.Path;
import java.util.concurrent.locks.Lock;

import org.springframework.transaction.TransactionStatus;

public class RepositoryStreamContext
{

    private Path path;

    private Lock lock;

    private boolean opened;

    private TransactionStatus transaction;

    private boolean artifactExists;

    private String lockKey;

    public Path getPath()
    {
        return path;
    }

    public void setPath(Path path)
    {
        this.path = path;
    }

    public Lock getLock()
    {
        return lock;
    }

    public void setLock(Lock lock)
    {
        this.lock = lock;
    }

    public boolean isOpened()
    {
        return opened;
    }

    public void setOpened(boolean opened)
    {
        this.opened = opened;
    }

    public TransactionStatus getTransaction()
    {
        return transaction;
    }

    public void setTransaction(TransactionStatus transaction)
    {
        this.transaction = transaction;
    }

    public boolean getArtifactExists()
    {
        return artifactExists;
    }

    public void setArtifactExists(boolean artifactExists)
    {
        this.artifactExists = artifactExists;
    }

    public String getLockKey() {
        return lockKey;
    }

    public void setLockKey(String lockKey) {
        this.lockKey = lockKey;
    }
}
