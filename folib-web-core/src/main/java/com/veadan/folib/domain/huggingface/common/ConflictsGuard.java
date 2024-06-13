package com.veadan.folib.domain.huggingface.common;

public interface ConflictsGuard<K> extends LockingMap<K> {
    ConflictGuard getLock(K paramK);
}
