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
package com.folib.scanner.service;

import com.folib.scanner.task.OptimizedDynamicPriorityBlockingQueue;
import com.folib.scanner.task.ScannerTask;
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
