package com.veadan.folib.ws.server;

import lombok.Getter;
import lombok.ToString;

import java.util.concurrent.Callable;

/**
 * @author pengYongQiang
 * @date 2024/2/20 14:30
 */

@Getter
@ToString
public abstract class RetryTask implements Callable<Boolean> {
    private final String taskId;
    private final int maxRetries;
    private int retryCount = 0;


    public RetryTask(String taskId) {
        this.taskId = taskId;
        this.maxRetries = 3; // 最多重试3次
    }

    @Override
    public Boolean call() throws Exception {
        while (retryCount < maxRetries) {
            try {
                exec(this);
                return true;
            } catch (Exception e) {
                retryCount++;
                System.out.println("Retry " + retryCount + " for task: " + taskId);
            }
        }
        System.out.println("Task " + taskId + " failed after " + retryCount + " attempts.");
        return false; // 任务失败
    }

    public abstract void exec(RetryTask retryTask);
}