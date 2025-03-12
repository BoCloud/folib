package com.veadan.folib.components.micrometer;

import com.datastax.oss.driver.shaded.guava.common.util.concurrent.AtomicDouble;
import lombok.Getter;
import lombok.Setter;

import java.util.concurrent.atomic.AtomicLong;
@Getter
@Setter
public class MetricData {
    // 使用占比
    private double percentage;

    private String packageType;

    private String type;

    private String name;

    private String storageId;

    private double value;

    private String metricName;
}
