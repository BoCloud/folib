package com.folib.io;

import org.springframework.transaction.TransactionStatus;

import java.nio.file.Path;
import java.util.concurrent.locks.Lock;

public class RepositoryStreamContext {

    private Path path;

    private Lock lock;

    private boolean opened;

    private TransactionStatus transaction;

    private boolean artifactExists;

    private Boolean isLocked;

    public Path getPath() {
        return path;
    }

    public void setPath(Path path) {
        this.path = path;
    }

    public Lock getLock() {
        return lock;
    }

    public void setLock(Lock lock) {
        this.lock = lock;
    }

    public boolean isOpened() {
        return opened;
    }

    public void setOpened(boolean opened) {
        this.opened = opened;
    }

    public TransactionStatus getTransaction() {
        return transaction;
    }

    public void setTransaction(TransactionStatus transaction) {
        this.transaction = transaction;
    }

    public boolean getArtifactExists() {
        return artifactExists;
    }

    public void setArtifactExists(boolean artifactExists) {
        this.artifactExists = artifactExists;
    }

    public Boolean getLocked() {
        return isLocked;
    }

    public void setLocked(Boolean locked) {
        isLocked = locked;
    }
}
