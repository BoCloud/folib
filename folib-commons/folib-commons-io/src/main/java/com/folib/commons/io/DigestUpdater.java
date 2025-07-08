package com.folib.commons.io;

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class DigestUpdater {
    private final int THREAD_NUMS = 4;
    // 静态线程池实现复用（根据CPU核心数初始化）
    private ExecutorService executorService = null;


    /**
     * Update the digests concurrently.
     *
     * @param digests
     * @param bytes
     * @param off
     * @param numberOfBytesRead
     */
    public void updateDigestsConcurrently(Map<?, MessageDigest> digests, byte[] bytes, int off, int numberOfBytesRead) {
        // 初始化线程池
        if (executorService == null) {
            executorService = Executors.newFixedThreadPool(THREAD_NUMS);
        }

        List<Future<?>> futures = new ArrayList<>(digests.size());

        // 提交所有任务到线程池
        for (Map.Entry<?, MessageDigest> entry : digests.entrySet()) {
            futures.add(executorService.submit(() -> {
                MessageDigest digest = entry.getValue();
                digest.update(bytes, off, numberOfBytesRead);
            }));
        }

        // 等待当前批次所有任务完成
        waitAllFutures(futures);
    }

    /**
     * 并发更新摘要信息（使用共享线程池）
     *
     * @param digests 需要更新的摘要集合
     * @param bytes   要计算的数据字节
     */
    public void updateDigestsConcurrently(Map<?, MessageDigest> digests, byte[] bytes) {
        // 初始化线程池
        if (executorService == null) {
            executorService = Executors.newFixedThreadPool(THREAD_NUMS);
        }

        List<Future<?>> futures = new ArrayList<>(digests.size());

        // 提交所有任务到线程池
        for (Map.Entry<?, MessageDigest> entry : digests.entrySet()) {
            futures.add(executorService.submit(() -> {
                MessageDigest digest = entry.getValue();
                digest.update(bytes);
            }));
        }

        // 等待当前批次所有任务完成
        waitAllFutures(futures);
    }

    /**
     * 等待所有Future完成（保持与原始方法相同的阻塞语义）
     *
     * @param futures 需要等待的Future集合
     */
    private void waitAllFutures(List<Future<?>> futures) {
        for (Future<?> future : futures) {
            try {
                future.get();  // 阻塞直到任务完成
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException("Task interrupted", e);
            } catch (ExecutionException e) {
                throw new RuntimeException("Task execution failed", e.getCause());
            }
        }
    }

    /**
     * 关闭线程池（显式调用）
     */
    public void clearThreadPool() {
        if (executorService != null) {
            executorService.shutdown();
            executorService = null;
        }
    }
}
