package com.veadan.folib.components.micrometer;

import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.mapper.StorageMonitoringMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author pj
 * @version 1.0
 * @date 2021/1/14 15:06
 */
@Component
public class MetricDataFetcher {
    private StorageMonitoringMapper storageMonitoringMapper;
    private final String METRIC_FILES = "folib.storage.repo.files";
    private final String METRIC_FOLDERS = "folib.storage.repo.folders";
    private final String METRIC_ITEMS = "folib.storage.repo.items";
    private final String METRIC_PERCENTAGE = "folib.storage.repo.percentage";
    private final String METRIC_ARTIFACTS_SIZE = "folib.storage.repo.artifacts.size";
    private final String metric_artifacts_count = "folib.storage.repo.artifacts.count";
    private final String METRIC_DISK_PERCENTAGE = "folib.storage.repo.disk.percentage";

    public MetricDataFetcher(StorageMonitoringMapper storageMonitoringMapper) {
        this.storageMonitoringMapper = storageMonitoringMapper;
    }

    @Scheduled(fixedRate = 60000) // 每5秒查询一次
    public List<MetricData> fetchAllMetrics() {
        return saveMetricData(storageMonitoringMapper.getTodayData());

    }

    /**
     * 保存指标数据
     *
     * @param storageMonitoringList
     * @return
     */
    private List<MetricData> saveMetricData(List<StorageMonitoring> storageMonitoringList) {
        if (storageMonitoringList != null && !storageMonitoringList.isEmpty()) {
            List<MetricData> metricDataList = new ArrayList<>();
            storageMonitoringList = storageMonitoringList.stream()
                    .filter(d -> d.getRepositoryId() != null && !d.getRepositoryId().isEmpty())
                    .collect(Collectors.toList());
            for (StorageMonitoring monitoring : storageMonitoringList) {
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_FILES, monitoring.getFilesCount()));
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_FOLDERS, monitoring.getFoldersCount()));
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_ITEMS, monitoring.getItemsCount()));
                double percentage = monitoring.getUsedStorageQuotaSizePercentage() == null ? 0 : monitoring.getUsedStorageQuotaSizePercentage().doubleValue();
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_PERCENTAGE, percentage));
                double artifactsSize = monitoring.getArtifactsSize() == null ? 0 : monitoring.getArtifactsSize().doubleValue();
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_ARTIFACTS_SIZE, artifactsSize));
                double artifactsCount = monitoring.getArtifactsCount() == null ? 0 : monitoring.getArtifactsCount().doubleValue();
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), metric_artifacts_count, artifactsCount));
                double diskPercentage = monitoring.getUsedStorageDeviceSizePercentage() == null ? 0 : monitoring.getUsedStorageDeviceSizePercentage().doubleValue();
                metricDataList.add(setMetric(monitoring.getStorageId(),monitoring.getRepositoryId(), monitoring.getRepositoryType(), monitoring.getRepositoryLayout(), METRIC_DISK_PERCENTAGE, diskPercentage));
            }
            return metricDataList;
        }
        return null;
    }

    /**
     * 设置指标数据
     *
     * @param repositoryId
     * @param type
     * @param repositoryLayout
     * @param metricName
     * @param value
     * @return
     */
    public MetricData setMetric(String storageId,String repositoryId, String type, String repositoryLayout, String metricName, double value) {
        MetricData metricData = new MetricData();
        metricData.setName(repositoryId);
        metricData.setType(type);
        metricData.setPackageType(repositoryLayout);
        metricData.setValue(value);
        metricData.setMetricName(metricName);
        metricData.setStorageId(storageId);
        return metricData;
    }
}