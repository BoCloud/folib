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
package com.folib.components.micrometer;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * @author pj
 * @date 2023/6/14 15:09
 * @description 自定义磁盘指标
 */
@Slf4j
@Component
public class DiskMetrics {

    @Autowired
    private MeterRegistry meterRegistry;

    //磁盘路径
    @Value("${folib.vault:/}")
    private String path;

    // 获取磁盘总空间
    @Getter
    private double totalSpace;
    // 获取磁盘空闲空间
    @Getter
    private double freeSpace;
    // 获取磁盘可用空间
    @Getter
    private double usableSpace;

    // 初始化时注册指标
    @PostConstruct
    public void initMetrics() {

        Gauge.builder("disk.total.bytes", this, DiskMetrics::getTotalSpace).register(meterRegistry);
        Gauge.builder("disk.free.bytes", this, DiskMetrics::getFreeSpace).register(meterRegistry);
        Gauge.builder("disk.usable.bytes", this, DiskMetrics::getUsableSpace).register(meterRegistry);
    }

    // 每隔 1 分钟更新一次磁盘空间指标
    @Scheduled(fixedRate = 60000)  // 每 60 秒更新一次
    public void updateMetrics() {
        Path dir = Paths.get(path);
        if (Files.exists(dir)) {
            try {
                FileStore fileStore = Files.getFileStore(Paths.get(path));
                this.totalSpace = fileStore.getTotalSpace();
                this.usableSpace = fileStore.getUsableSpace();
                this.freeSpace = this.totalSpace - this.usableSpace;
            } catch (IOException e) {
                log.error("Failed to update disk metrics: {}", e.getMessage());
            }
        } else {
            log.error("Root directory does not exist or is not accessible.");
        }
    }
}