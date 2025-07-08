package com.folib.scanner.task;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Data
@Slf4j
public class AnalyzeSbomTask implements Runnable, Comparable<AnalyzeSbomTask>{

    private  String taskId;
    private Runnable task;
    private int priority;

    public AnalyzeSbomTask(int priority, String taskId, Runnable task) {

        this.priority = priority;
        this.taskId = taskId;
        this.task = task;
    }

    @Override
    public void run() {
        log.info("AnalyzeSbomTask run taskId [{}] begin run", taskId);
        task.run();
    }

    @Override
    public int compareTo( AnalyzeSbomTask o) {
        return Integer.compare(this.priority, o.priority);
    }

}
