package com.folib.scanner.task;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * @author veadan
 **/
@Data
@Slf4j
public class ScannerTask implements Runnable, Comparable<ScannerTask> {

    /**
     * 任务id
     */
    private String taskId;
    /**
     * 优先级
     */
    private int priority;
    /**
     * 任务
     */
    private Runnable task;

    public ScannerTask(int priority, String taskId, Runnable task) {
        this.priority = priority;
        this.taskId = taskId;
        this.task = task;
    }

    @Override
    public void run() {
        log.info("ScannerTask run taskId [{}] begin run", taskId);
        task.run();
    }

    @Override
    public int compareTo(ScannerTask other) {
        return Integer.compare(this.priority, other.priority);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        ScannerTask task = (ScannerTask) obj;
        return taskId.equals(task.getTaskId());
    }

    @Override
    public int hashCode() {
        return taskId.hashCode();
    }
}
