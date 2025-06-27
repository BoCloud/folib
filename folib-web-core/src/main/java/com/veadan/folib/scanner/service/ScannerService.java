package com.veadan.folib.scanner.service;

import com.veadan.folib.scanner.task.ScannerTask;
import com.veadan.folib.ws.task.OptimizedDynamicPriorityBlockingQueue;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.concurrent.Semaphore;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class ScannerService {

    public static final Semaphore SEMAPHORE = new Semaphore(Runtime.getRuntime().availableProcessors() * 2);

    /**
     * 创建优先级队列，使用自然排序
     */
    private final OptimizedDynamicPriorityBlockingQueue<ScannerTask> queue = new OptimizedDynamicPriorityBlockingQueue<>(Comparator.naturalOrder());

    /**
     * 添加任务
     *
     * @param task 扫描任务
     */
    public void addTask(ScannerTask task) {
        log.info("Add task taskId [{}] priority [{}] available permits [{}]", task.getTaskId(), task.getPriority(), SEMAPHORE.availablePermits());
        queue.put(task, task.getTaskId());
        log.info("Add task taskId [{}] priority [{}] size [{}]", task.getTaskId(), task.getPriority(), getQueueSize());
    }

    /**
     * 更新任务优先级
     *
     * @param taskId      任务ID
     * @param newPriority 新优先级
     */
    public void updateTaskPriority(String taskId, int newPriority) {
        ScannerTask taskToAdjust = queue.getElementById(taskId);
        if (taskToAdjust != null) {
            taskToAdjust.setPriority(newPriority);
            log.info("Update task priority [{}] priority [{}]", taskToAdjust.getTaskId(), taskToAdjust.getPriority());
            queue.adjustPriority(taskToAdjust, Comparator.comparingInt(ScannerTask::getPriority).reversed());
        }
        throw new RuntimeException("Task not found taskId:" + taskId);
    }

    /**
     * 删除任务优先级
     *
     * @param taskId 任务ID
     */
    public void deleteTask(String taskId) {
        ScannerTask taskToAdjust = queue.getElementById(taskId);
        if (taskToAdjust != null) {
            log.info("Delete task priority [{}] priority [{}]", taskToAdjust.getTaskId(), taskToAdjust.getPriority());
            queue.remove(taskToAdjust);
        }
    }

    /**
     * 获取下一个任务
     *
     * @return ScannerTask
     */
    public ScannerTask getNextTask() {
        ScannerTask task = null;
        try {
            log.info("Current queue size [{}]", getQueueSize());
            //获取信号量
            acquire();
            task = queue.take();
            if (task != null) {
                log.info("Get next task after queue size [{}]", getQueueSize());
            }
        } catch (Exception e) {
            log.error("InterruptedException [{}] " + ExceptionUtils.getStackTrace(e));
        }
        return task;
    }

    /**
     * 获取队列大小
     *
     * @return int
     */
    public int getQueueSize() {
        return queue.size();
    }

    /**
     * 获取信号量
     *
     * @throws InterruptedException 异常
     */
    public void acquire() throws InterruptedException {
        SEMAPHORE.acquire();
        log.info("Acquire after semaphore [{}]", SEMAPHORE.availablePermits());
    }

    /**
     * 释放信号量
     */
    public void release() {
        SEMAPHORE.release();
        log.info("Release after semaphore [{}]", SEMAPHORE.availablePermits());
    }
}
