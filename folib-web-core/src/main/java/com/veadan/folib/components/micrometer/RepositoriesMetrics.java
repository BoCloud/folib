package com.veadan.folib.components.micrometer;

import com.alibaba.fastjson.JSON;
import com.datastax.oss.driver.shaded.guava.common.util.concurrent.AtomicDouble;
import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.mapper.StorageMonitoringMapper;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tags;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.ToDoubleFunction;
import java.util.stream.Collectors;

@Slf4j
@Component
//public class RepositoriesMetrics {
//
//    @Autowired
//    private MeterRegistry meterRegistry;
//
//    @Autowired
//    private StorageMonitoringMapper storageMonitoringMapper;
//
//    //文件数量
//    @Getter
//    private long files;
//
//    //文件夹数量
//    @Getter
//    private long folders;
//
//    //artifactory_storage_repo_items
//    @Getter
//    private long items;
//
//    //artifactory_storage_repo_used_bytes
//    @Getter
//    private double usedBytes;
//
//    //artifactory_storage_repo_percentage
//    @Getter
//    private double percentage;
//
//    @PostConstruct
//    public void initMetrics() {
//        List<StorageMonitoring> monitorings = storageMonitoringMapper.getTodayData();
//        monitorings = monitorings.stream().filter(d-> d.getRepositoryId()!=null).collect(Collectors.toList());
//        for (StorageMonitoring item : monitorings) {
//            updataData(item);
//        }
//    }
//
//    @Scheduled(fixedRate = 60000)  // 每 60 秒更新一次
//    public void updateMetrics() {
//        initMetrics();
//    }
//
//    //package_type,type,name
//    public void updataData(StorageMonitoring monitoring) {
//        this.files = monitoring.getFilesCount();
//        this.folders = monitoring.getFoldersCount();
//        this.items = monitoring.getItemsCount();
//        this.usedBytes = monitoring.getUsedStorageDeviceSize() == null ? 0 : monitoring.getUsedStorageDeviceSize().doubleValue();
//        this.percentage = monitoring.getUsedStorageDeviceSizePercentage() == null ? 0 : monitoring.getUsedStorageDeviceSizePercentage().doubleValue();
//        log.info("StorageMonitoring:{}", JSON.toJSONString(monitoring));
//        Tags tags = Tags.of("package_type", monitoring.getRepositoryLayout(), "type", monitoring.getStorageDeviceType(), "name", monitoring.getRepositoryId());
//        Gauge.builder("folib.storage.repo.files", this, RepositoriesMetrics::getFiles)
//                .tags(tags)
//                .register(meterRegistry);
//        Gauge.builder("folib.storage.repo.folders", this, RepositoriesMetrics::getFolders)
//                .tags(tags)
//                .register(meterRegistry);
//        Gauge.builder("folib.storage.repo.items", this, RepositoriesMetrics::getItems)
//                .tags(tags)
//                .register(meterRegistry);
//        Gauge.builder("folib.storage.repo.used.bytes", this, RepositoriesMetrics::getUsedBytes)
//                .tags(tags)
//                .register(meterRegistry);
//        Gauge.builder("folib.storage.repo.percentage", this, RepositoriesMetrics::getPercentage)
//                .tags(tags)
//                .register(meterRegistry);
//    }
//};

public class RepositoriesMetrics {

    @Autowired
    private MeterRegistry meterRegistry;

    @Autowired
    private StorageMonitoringMapper storageMonitoringMapper;

    // 文件数量
    @Getter
    private AtomicLong files = new AtomicLong(0);

    // 文件夹数量
    @Getter
    private AtomicLong folders = new AtomicLong(0);

    // 条目数量
    @Getter
    private AtomicLong items = new AtomicLong(0);

    // 已使用量
    @Getter
    private AtomicDouble usedBytes = new AtomicDouble(0.0);

    // 使用占比
    @Getter
    private AtomicDouble percentage = new AtomicDouble(0.0);

    private final Map<String, Gauge> gauges = new ConcurrentHashMap<>();

    @PostConstruct
    public void initMetrics() {
        try {
            List<StorageMonitoring> monitorings = storageMonitoringMapper.getTodayData();
            for (StorageMonitoring item : filterValidMonitorings(monitorings)) {
                updateData(item);
                registerGauges(item);
            }
        } catch (Exception e) {
            log.error("Failed to initialize metrics", e);
        }
    }

    @Scheduled(fixedRate = 60000)  // 每 60 秒更新一次
    public void updateMetrics() {
        try {
            initMetrics();
        } catch (Exception e) {
            log.error("Failed to update metrics", e);
        }
    }

    private List<StorageMonitoring> filterValidMonitorings(List<StorageMonitoring> monitorings) {
        return monitorings.stream()
                .filter(d -> d.getRepositoryId() != null && !d.getRepositoryId().isEmpty())
                .collect(Collectors.toList());
    }

    /**
     * 更新数据
     * @param monitoring
     */
    public synchronized void updateData(StorageMonitoring monitoring) {
        this.files.set(monitoring.getFilesCount());
        this.folders.set(monitoring.getFoldersCount());
        this.items.set(monitoring.getItemsCount());
        this.usedBytes.set(monitoring.getUsedStorageDeviceSize() == null ? 0 : monitoring.getUsedStorageDeviceSize().doubleValue());
        this.percentage.set(monitoring.getUsedStorageDeviceSizePercentage() == null ? 0 : monitoring.getUsedStorageDeviceSizePercentage().doubleValue());
    }

    private void registerGauges(StorageMonitoring monitoring) {
        Tags tags = Tags.of(
                "package_type", monitoring.getRepositoryLayout(),
                "type", monitoring.getStorageDeviceType(),
                "name", monitoring.getRepositoryId()
        );
        registerGauge("folib.storage.repo.files", RepositoriesMetrics::getFilesValue, tags);
        registerGauge("folib.storage.repo.folders", RepositoriesMetrics::getFoldersValue, tags);
        registerGauge("folib.storage.repo.items", RepositoriesMetrics::getItemsValue, tags);
        registerGauge("folib.storage.repo.used.bytes", RepositoriesMetrics::getUsedBytesValue, tags);
        registerGauge("folib.storage.repo.percentage", RepositoriesMetrics::getPercentageValue, tags);
    }

    private void registerGauge(String name, ToDoubleFunction<RepositoriesMetrics> function, Tags tags) {
        if (!gauges.containsKey(name)) {
            Gauge.builder(name, this, function)
                    .tags(tags)
                    .register(meterRegistry);
            gauges.put(name, meterRegistry.find(name).gauge());
        }
    }

    private double getFilesValue() {
        return files.get();
    }

    private double getFoldersValue() {
        return folders.get();
    }

    private double getItemsValue() {
        return items.get();
    }

    private double getUsedBytesValue() {
        return usedBytes.get();
    }

    private double getPercentageValue() {
        return percentage.get();
    }
}