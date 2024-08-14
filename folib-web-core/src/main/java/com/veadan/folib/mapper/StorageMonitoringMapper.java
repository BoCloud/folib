package com.veadan.folib.mapper;

import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.scanner.common.base.CommonMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface StorageMonitoringMapper extends CommonMapper<StorageMonitoring> {

    /**
     * 批量保存存储监控数据
     *
     * @param storageMonitoringList 字典列表
     */
    void batchInsertStorageMonitoring(@Param("storageMonitoringList") List<StorageMonitoring> storageMonitoringList);

}
