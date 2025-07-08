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