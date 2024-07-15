package com.veadan.folib.ws.server;

import org.openxmlformats.schemas.wordprocessingml.x2006.main.STHeightRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 动态优先级任务
 */
public class DynamicPriorityTask implements Callable<Void>, Comparable<DynamicPriorityTask> {

    private static final Logger log = LoggerFactory.getLogger(DynamicPriorityTask.class);

    //优先级
    private final AtomicInteger priority = new AtomicInteger();


    private final String taskId;

    //是否执行
    private AtomicBoolean isExecuted = new AtomicBoolean(false);

    //优先级任务
    private ComparableFutureTask futureTask;

    //执行函数
    private Runnable executeFunction;

    /**
     * 构造函数
     *
     * @param initialPriority 初始优先级
     * @param taskId           任务ID
     * @param callback        执行函数
     */
    public DynamicPriorityTask(int initialPriority, String taskId, Runnable callback) {
        this.priority.set(initialPriority);
        this.taskId = taskId;
        this.executeFunction = callback;
    }

    public void setPriority(int newPriority) {
        this.priority.set(newPriority);
    }

    public void setFutureTask(ComparableFutureTask futureTask) {
        this.futureTask = futureTask;
    }

    public ComparableFutureTask getFutureTask() {
        return futureTask;
    }

    public String getTaskId() {
		return taskId;
	}


    @Override
    public int compareTo(DynamicPriorityTask other) {
        return Integer.compare(this.priority.get(), other.priority.get());
    }

    @Override
    public Void call() {
        if (!isExecuted.get()) {
            isExecuted.set(true);
            executeFunction.run();
            log.info("Executing taskId:{} ,name:{} ,with priority:{} " , taskId , Thread.currentThread().getName(), priority.get());

        }
        return null;
    }
}
