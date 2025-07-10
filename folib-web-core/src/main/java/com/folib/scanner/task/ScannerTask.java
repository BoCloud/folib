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
