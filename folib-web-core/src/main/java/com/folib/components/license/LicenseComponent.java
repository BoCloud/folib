package com.folib.components.license;

import cn.hutool.core.date.DateUtil;
import com.folib.entity.License;
import com.folib.services.LicenseService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class LicenseComponent {

    private List<License> licenseList;

    @Autowired
    private LicenseService licenseService;

    /**
     * 获取所有license
     *
     * @return license
     */
    public List<License> getLicenses() {
        if (CollectionUtils.isEmpty(licenseList)) {
            log.info("获取所有license [{}]", DateUtil.now());
            licenseList = licenseService.getLicenseCache();
        }
        return licenseList;
    }
}
