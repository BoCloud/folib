package com.veadan.folib.domain.huggingface.common;

import java.util.Set;
import java.util.concurrent.TimeUnit;

public interface LockingMap<T> {
    boolean tryToLock(T paramT, long paramLong, TimeUnit paramTimeUnit) throws InterruptedException;

    void unlock(T paramT);

    void forceUnlock(T paramT);

    boolean isLocked(T paramT);

    int size();

    Set<T> keySet();
}
