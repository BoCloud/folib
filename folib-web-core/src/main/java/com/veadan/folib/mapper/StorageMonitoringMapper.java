package com.veadan.folib.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.StorageMonitoring;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface StorageMonitoringMapper extends BaseMapper<StorageMonitoring> {

    /**
     * 批量保存存储监控数据
     *
     * @param storageMonitoringList 字典列表
     */
    void batchInsertStorageMonitoring(@Param("storageMonitoringList") List<StorageMonitoring> storageMonitoringList);

    /**
     * 获取今天的数据
     * @return List<StorageMonitoring>
     */
    List<StorageMonitoring>  getTodayData();
}
