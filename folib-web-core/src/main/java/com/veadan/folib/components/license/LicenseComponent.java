package com.veadan.folib.components.license;

import cn.hutool.core.date.DateUtil;
import com.veadan.folib.entity.License;
import com.veadan.folib.services.LicenseService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
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
            licenseList = licenseService.selectLicense(License.builder().isDeprecated(0).build());
        }
        return licenseList;
    }
}
