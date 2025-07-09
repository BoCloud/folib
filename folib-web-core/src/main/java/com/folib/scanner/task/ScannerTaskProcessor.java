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

import com.folib.scanner.service.ScannerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.time.Duration;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ScannerTaskProcessor {

    private final ScannerService scannerService;
    private final Sinks.Many<ScannerTask> sink;
    private final Scheduler ioScheduler;
    private final int cpuCores;

    /**
     * 初始化扫描服务、任务执行器和任务触发器
     *
     * @param scannerService 注入的 ScannerService 实例
     */
    @Autowired
    public ScannerTaskProcessor(ScannerService scannerService) {
        this.scannerService = scannerService;
        //支持多个订阅者（多播），并且使用缓冲区来处理背压
        this.sink = Sinks.many().multicast().onBackpressureBuffer();
        cpuCores = Runtime.getRuntime().availableProcessors() * 2;
        // 配置一个适用于 IO 密集型任务的 Scheduler
        ioScheduler = Schedulers.boundedElastic();
        initializeTaskProcessing();
    }

    /**
     * 初始化任务处理流程
     * 配置并行处理任务的逻辑，并启动任务轮询
     */
    private void initializeTaskProcessing() {
        sink.asFlux()
                // 并发数量控制
                .parallel(cpuCores)
                // 确保使用正确的调度器
                .runOn(ioScheduler)
                // 处理每个轨道中的任务
                .doOnNext(task -> {
                    log.info("Task execution begin taskId [{}] ", task.getTaskId());
                    try {
                        task.run();
                        log.info("Task execution completed taskId [{}] ", task.getTaskId());
                    } catch (Exception e) {
                        log.error("InitializeTaskProcessing error [{}]", ExceptionUtils.getStackTrace(e));
                    } finally {
                        scannerService.release();
                    }
                })
                // 合并轨道结果
                .sequential()
                // 订阅任务处理逻辑
                .subscribe();
        // 启动任务轮询
        pollForTasks();
    }

    /**
     * 轮询任务
     * 定期从 ScannerService 获取任务，并通过 Sinks 推送到处理流中
     */
    private void pollForTasks() {
        // 动态获取任务
        Mono.defer(() -> Mono.justOrEmpty(scannerService.getNextTask()))
                // 无任务时延迟 1000 毫秒后重试
                .repeatWhenEmpty(repeat -> repeat.delayElements(Duration.ofMillis(1000)))
                // 推送任务到处理流中
                //.doOnNext(sink::tryEmitNext)
                .subscribeOn(ioScheduler)
                // 推送任务到处理流中
                .doOnNext(task -> {
                    // 捕获 tryEmitNext 的返回值
                    Sinks.EmitResult result = sink.tryEmitNext(task);
                    // 根据结果输出日志
                    if (result.isSuccess()) {
                        log.info("Push task success taskId [{}] ", task.getTaskId());
                    } else {
                        log.error("Push task error taskId [{}] reason [{}]", task.getTaskId(), result);
                    }
                })
                // 无限次重复获取任务
                .repeat()
                // 在独立线程池中执行
                .subscribe();
    }
}
