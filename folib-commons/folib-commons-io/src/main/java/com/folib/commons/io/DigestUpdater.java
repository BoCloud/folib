/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
