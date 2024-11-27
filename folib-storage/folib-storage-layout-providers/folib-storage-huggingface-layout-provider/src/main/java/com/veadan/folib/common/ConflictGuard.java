package com.veadan.folib.common;

import java.util.concurrent.TimeUnit;

public interface ConflictGuard {
    boolean tryToLock(long paramLong, TimeUnit paramTimeUnit) throws InterruptedException;

    void unlock();

    void forceUnlock();

    boolean isLocked();
}
