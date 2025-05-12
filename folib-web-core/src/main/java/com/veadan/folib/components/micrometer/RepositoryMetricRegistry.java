package com.veadan.folib.components.micrometer;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Component
public class RepositoryMetricRegistry {
    private static final Logger logger = LoggerFactory.getLogger(RepositoryMetricRegistry.class);
    private final MeterRegistry meterRegistry;
    private final MetricDataFetcher metricDataFetcher;
    // 缓存所有指标，Key格式：metricName|packageType|type|name
    private final Map<String, AtomicReference<Double>> metricCache = new ConcurrentHashMap<>();

    public RepositoryMetricRegistry(MeterRegistry meterRegistry, MetricDataFetcher metricDataFetcher) {
        this.meterRegistry = meterRegistry;
        this.metricDataFetcher = metricDataFetcher;
        init();
    }

    @PostConstruct
    public void init() {
        // 初始加载指标
        updateAllMetrics();
        // 定时更新
        ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
        scheduler.scheduleAtFixedRate(this::updateAllMetrics, 0, 60, TimeUnit.SECONDS);
    }

    private void updateAllMetrics() {
        Instant startTime = Instant.now();
        List<MetricData> metrics = metricDataFetcher.fetchAllMetrics();
        if(metrics==null || metrics.isEmpty()){
            logger.info("No metrics fetched at {}", startTime);
            return;
        }
        metrics.forEach(this::updateSingleMetric);
        cleanupExpiredMetrics(metrics);
        logger.info("Metrics updated at {}", startTime);
    }

    /**
     * 更新或创建单个指标
     */
    private void updateSingleMetric(MetricData data) {
        String uniqueKey = String.join("|",
                data.getMetricName(),
                data.getPackageType(),
                data.getType(),
                data.getName()
        );

        AtomicReference<Double> valueRef = metricCache.computeIfAbsent(uniqueKey, k -> {
            // 注册新指标
            AtomicReference<Double> ref = new AtomicReference<>(data.getValue());
            Gauge.builder(data.getMetricName(), ref, AtomicReference::get)
                    .tags(
                            "package_type", data.getPackageType(),
                            "type", data.getType(),
                            "name", data.getName(),
                            "storageId", data.getStorageId()
                    )
                    .register(meterRegistry);
            return ref;
        });
        valueRef.set(data.getValue());
    }


    /**
     * 清理已删除的指标
     */
    private void cleanupExpiredMetrics(List<MetricData> activeMetrics) {
        Set<String> activeKeys = activeMetrics.stream()
                .map(data -> String.join("|",
                        data.getMetricName(),
                        data.getPackageType(),
                        data.getType(),
                        data.getName()
                ))
                .collect(Collectors.toSet());

        metricCache.keySet().removeIf(key -> {
          return  !activeKeys.contains(key);
        });
    }
}

