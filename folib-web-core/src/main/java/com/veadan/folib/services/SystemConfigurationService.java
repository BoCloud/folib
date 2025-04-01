package com.veadan.folib.services;

import com.veadan.folib.model.request.ExportSystemConfigurationReq;
import com.veadan.folib.model.request.ImportSystemConfigurationReq;

/**
 * @author leipenghui
 * @date 2025/3/28
 **/
public interface SystemConfigurationService {

    /**
     * 系统配置导出
     * @param exportSystemConfiguration 参数
     */
    void exportSystemConfiguration(ExportSystemConfigurationReq exportSystemConfiguration);

    /**
     * 系统配置导入
     * @param importSystemConfiguration 参数
     */
    void importSystemConfiguration(ImportSystemConfigurationReq importSystemConfiguration);
}
