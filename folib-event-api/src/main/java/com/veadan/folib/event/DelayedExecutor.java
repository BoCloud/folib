package com.veadan.folib.event;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.eclipse.jetty.util.thread.ThreadPool;
import org.eclipse.jetty.util.thread.ThreadPool.SizedThreadPool;

/**
 * @author sbespalov
 */
public class DelayedExecutor implements Executor
{

    private final Executor executor;
    private final ScheduledExecutorService scheduler;

    public DelayedExecutor(Executor executor)
    {
        this.executor = executor;
        this.scheduler = Executors.newScheduledThreadPool(((SizedThreadPool)executor).getMaxThreads());
    }

    @Override
    public void execute(Runnable command)
    {
        scheduler.schedule(() -> executor.execute(command), 10, TimeUnit.SECONDS);
    }

}
