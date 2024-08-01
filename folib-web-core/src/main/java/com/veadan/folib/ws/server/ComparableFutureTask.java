package com.veadan.folib.ws.server;

import java.util.concurrent.FutureTask;

/**
 *
 */
public class ComparableFutureTask extends FutureTask<Void> implements Comparable<ComparableFutureTask> {

    private final DynamicPriorityTask task;

    public ComparableFutureTask(DynamicPriorityTask task) {
        super(task);
        this.task = task;
    }

    @Override
    public int compareTo(ComparableFutureTask other) {
        return this.task.compareTo(other.task);
    }

    public DynamicPriorityTask getTask() {
        return task;
    }
}
