package com.veadan.folib.ws.server;

import cn.hutool.core.thread.ThreadFactoryBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.*;
import java.util.Map;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * 任务队列管理v2
 */
public class TaskQueueV2Manager {

    private static final Logger log = LoggerFactory.getLogger(TaskQueueV2Manager.class);

    /**
     * 优先级队列
     */
    private final PriorityBlockingQueue<ComparableFutureTask> priorityQueue;

    /**
     * 线程池
     */
    private final ThreadPoolExecutor executor;

    /**
     * 任务映射
     */
    private final Map<String, DynamicPriorityTask> taskMap;

    /**
     * 标记正在关闭
     */
    private volatile boolean isShuttingDown = false;
    /**
     * 锁
     */
    private final Lock lock = new ReentrantLock();
    /**
     * 线程池关闭时，等待队列为空
     */
    private final Condition notEmpty = lock.newCondition();

    /**
     * 构造函数
     *
     * @param threadNamePrefix 线程名前缀
     * @param queueSize        队列大小
     * @param thread           线程数
     */
    public TaskQueueV2Manager(String threadNamePrefix, int queueSize, int thread) {
        priorityQueue = new PriorityBlockingQueue<>(queueSize);
        ThreadFactory threadFactory = ThreadFactoryBuilder.create().setNamePrefix(threadNamePrefix).build();
        executor = new ThreadPoolExecutor(
                thread,
                thread,
                0L,
                TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>(), threadFactory, new ThreadPoolExecutor.AbortPolicy()
        ) {
            @Override
            protected void beforeExecute(Thread t, Runnable r) {
                super.beforeExecute(t, r);
                if (r instanceof ComparableFutureTask) {
                    String taskId = ((ComparableFutureTask) r).getTask().getTaskId();
                    // 从优先级队列中移除
                    priorityQueue.remove(r);
                    taskMap.remove(taskId);
                }
            }

            @Override
            protected void afterExecute(Runnable r, Throwable t) {
                super.afterExecute(r, t);
                if (r instanceof ComparableFutureTask) {
                    lock.lock();
                    try {
                        // 通知等待的线程
                        notEmpty.signalAll();
                    } finally {
                        lock.unlock();
                    }
                }
            }


        };
        taskMap = new ConcurrentHashMap<>();


        // 启动一个线程来从优先级队列中取任务并提交给线程池执行
        new Thread(() -> {
            while (true) {
                try {
                    lock.lock();
                    try {
                        while (priorityQueue.isEmpty() && !isShuttingDown) {
                            notEmpty.await();
                        }
                        if (isShuttingDown && priorityQueue.isEmpty()) {
                            break;
                        }
                    } finally {
                        lock.unlock();
                    }
                    ComparableFutureTask task = priorityQueue.take();
                    executor.execute(task);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                } catch (RejectedExecutionException e) {
                    // 在任务被拒绝时跳过当前任务，检查线程池是否正在关闭
                    if (isShuttingDown) {
                        break;
                    }
                }
            }
        }).start();
    }

    /**
     * 提交任务
     *
     * @param taskId          任务id
     * @param initialPriority 初始优先级
     * @param callback        回调
     */
    public void submitTask(String taskId, Priority initialPriority, Runnable callback) {
        DynamicPriorityTask task = new DynamicPriorityTask(initialPriority.getValue(), taskId, callback);
        taskMap.put(taskId, task);
        lock.lock();
        try {
            priorityQueue.offer(task.getFutureTask());
            // 通知调度线程有新任务加入
            notEmpty.signalAll();
        } finally {
            lock.unlock();
        }
    }

    /**
     * 更新任务优先级
     *
     * @param taskId      任务id
     * @param newPriority 新优先级
     */
    public void updateTaskPriority(String taskId, int newPriority) {
        DynamicPriorityTask task = taskMap.get(taskId);
        if (task != null) {
            log.info("Updating priority of  taskName:{} to {} ,taskId:{}", Thread.currentThread(), newPriority, taskId);
            task.setPriority(newPriority);
            // 触发队列重新排序
            lock.lock();
            try {
                priorityQueue.remove(task.getFutureTask());
                priorityQueue.offer(task.getFutureTask());
                // 通知调度线程有新任务加入
                notEmpty.signalAll();
            } finally {
                lock.unlock();
            }
        }
    }

    /**
     * 关闭线程池
     *
     * @throws InterruptedException
     */
    public void shutdownExecutor() throws InterruptedException {
        lock.lock();
        try {
            // 标记正在关闭
            isShuttingDown = true;
            // 通知调度线程
            notEmpty.signalAll();
        } finally {
            lock.unlock();
        }
        executor.shutdown();
        executor.awaitTermination(60, TimeUnit.SECONDS);
    }

    public synchronized void shutdownAndCancelTasks() {
        lock.lock();
        try {
            // 标记正在关闭
            isShuttingDown = true;
            // 通知调度线程
            notEmpty.signalAll();
        } finally {
            lock.unlock();
        }
        List<Runnable> awaitingTasks = executor.shutdownNow();
        log.info("cancelled {} awaiting task", awaitingTasks.size());
    }

}
