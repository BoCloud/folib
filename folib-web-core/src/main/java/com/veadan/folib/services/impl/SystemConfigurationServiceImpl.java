package com.veadan.folib.services.impl;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.model.request.ExportSystemConfigurationReq;
import com.veadan.folib.model.request.ImportSystemConfigurationReq;
import com.veadan.folib.services.SystemConfigurationService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author leipenghui
 * @date 2025/3/28
 **/
@Slf4j
@Service
public class SystemConfigurationServiceImpl implements SystemConfigurationService {

    @Autowired
    private PropertiesBooter propertiesBooter;

    @Override
    public void exportSystemConfiguration(ExportSystemConfigurationReq exportSystemConfiguration) {
        try {
            Path path = Path.of(exportSystemConfiguration.getPath());
            Files.createDirectories(path);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void importSystemConfiguration(ImportSystemConfigurationReq importSystemConfiguration) {

    }
}
