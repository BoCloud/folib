package com.veadan.folib.services;

import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.model.request.StorageMonitoringReq;
import com.veadan.folib.model.response.StorageMonitoringRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface StorageMonitoringService {

    /**
     * 更新存储监控数据
     */
    void updateStorageMonitoringData();

    /**
     * 新增存储监控
     *
     * @param storageMonitoring 存储监控
     */
    void saveStorageMonitoring(StorageMonitoring storageMonitoring);

    /**
     * 修改存储监控
     *
     * @param storageMonitoring 存储监控
     */
    void updateStorageMonitoring(StorageMonitoring storageMonitoring);

    /**
     * 查询存储监控
     *
     * @param storageMonitoring 存储监控
     * @return 存储监控列表
     */
    List<StorageMonitoringRes> queryStorageMonitoring(StorageMonitoring storageMonitoring);

    /**
     * 查询存储监控
     *
     * @param storageMonitoring 存储监控
     * @return 存储监控
     */
    StorageMonitoringRes queryOneStorageMonitoring(StorageMonitoring storageMonitoring);

    /**
     * 分页查询存储监控列表
     *
     * @param storageMonitoringReq 存储监控
     * @return 存储监控列表
     */
    TableResultResponse<StorageMonitoringRes> queryStorageMonitoringPage(StorageMonitoringReq storageMonitoringReq);
}
