package com.veadan.folib.ws.server;

import lombok.Getter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.Callable;

/**
 * @author pengYongQiang
 * @date 2024/2/20 14:30
 */

@Getter
@ToString
@Slf4j
public abstract class RetryTask implements Callable<Void> {
    private final int maxRetries;
    private int retryCount = 0;


    public RetryTask() {
        this(3);
    }

    public RetryTask(int maxRetries) {
        this.maxRetries = maxRetries;
    }

    @Override
    public Void call() throws Exception {
        while (retryCount < maxRetries) {
            try {
                exec(this);
                return null;
            } catch (Exception e) {
                retryCount++;
                log.warn("Retry " + retryCount + " for task: " + this, e);
                if (retryCount == maxRetries) {
                    log.info("Task " + this + " failed after " + retryCount + " attempts.");
                    throw e;
                }
            }
        }
        return null;
    }

    protected abstract void exec(RetryTask retryTask) throws Exception;
}