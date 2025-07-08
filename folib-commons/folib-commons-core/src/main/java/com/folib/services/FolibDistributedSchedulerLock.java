package com.folib.services;

public interface FolibDistributedSchedulerLock {

    Boolean getLock(String name, Long lockAtMostSeconds);

    int releaseLock(String name);
}
