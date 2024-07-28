package com.veadan.folib.ws.task;

import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 分发任务
 */
@Data
public class DistributionTask implements Runnable, Comparable<DistributionTask> {
    private static final Logger logger = LoggerFactory.getLogger(DistributionTask.class);
    private String taskId;
    private int priority;
    private Runnable task;

    public DistributionTask(int priority, String taskId, Runnable task) {
        this.priority = priority;
        this.taskId = taskId;
        this.task = task;
    }

    @Override
    public void run() {
        logger.info("DistributionTask run taskId:{}", taskId);
        task.run();
    }

    @Override
    public int compareTo(DistributionTask other) {
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
        DistributionTask task = (DistributionTask) obj;
        return taskId.equals(task.getTaskId());
    }

    @Override
    public int hashCode() {
        return taskId.hashCode();
    }

}
