package com.veadan.folib.domain.huggingface.common;

import java.util.concurrent.TimeUnit;

public interface ConflictGuard {
    boolean tryToLock(long paramLong, TimeUnit paramTimeUnit) throws InterruptedException;

    void unlock();

    void forceUnlock();

    boolean isLocked();
}
