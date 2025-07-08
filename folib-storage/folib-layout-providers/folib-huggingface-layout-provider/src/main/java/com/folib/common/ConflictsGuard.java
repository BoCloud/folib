package com.folib.common;

public interface ConflictsGuard<K> extends LockingMap<K> {
    ConflictGuard getLock(K paramK);
}
