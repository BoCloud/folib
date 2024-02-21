package com.veadan.folib.ws.server;

import cn.hutool.core.thread.ThreadFactoryBuilder;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.concurrent.*;

/**
 * @author pengYongQiang
 * @date 2024/2/20 14:30
 * <p>
 * 用一个单线程池模拟一个串行可动态添加取消任务的任务队列
 */
@Slf4j
public class TaskQueueManager {
    private final ExecutorService executorService;
    private final ConcurrentHashMap<String, Future<?>> taskMap = new ConcurrentHashMap<>();

    public TaskQueueManager(String threadNamePrefix,int queueSize) {
        ThreadFactory threadFactory = ThreadFactoryBuilder.create().setNamePrefix(threadNamePrefix).build();
        executorService = new ThreadPoolExecutor(1, 1,
                0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>(queueSize), threadFactory, new ThreadPoolExecutor.AbortPolicy());
    }

    public synchronized String submitTask(String taskId, Runnable runnable) {
        if (taskMap.containsKey(taskId)) {
            throw new IllegalArgumentException("Task with ID " + taskId + " already submitted.");
        }
        Future<?> future = executorService.submit(runnable);
        taskMap.put(taskId, future);
        log.info("Task " + taskId + " submitted.");
        return taskId;
    }

    public synchronized boolean cancelTask(String taskId) {
        Future<?> future = taskMap.get(taskId);
        if (future != null) {
            boolean cancelled = future.cancel(true); // 尝试取消任务
            if (cancelled) {
                taskMap.remove(taskId); // 从映射中移除已取消的任务
                log.info("Task " + taskId + " cancelled.");
            }
            return cancelled;
        }
        return false;
    }

    public synchronized void shutdownAndCancelTasks() {
        taskMap.values().forEach(booleanFuture -> booleanFuture.cancel(true));
        taskMap.clear();
        List<Runnable> awaitingTasks = executorService.shutdownNow();
        log.info("cancelled {} awaiting task", awaitingTasks.size());
    }
}