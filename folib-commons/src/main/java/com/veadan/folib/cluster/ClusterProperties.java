package com.veadan.folib.cluster;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Component
@Order
public class ClusterProperties {
    private static final Logger logger = LoggerFactory.getLogger(ClusterProperties.class);

    @Value("${folib.cluster.openflag:false}")
    private Boolean openFlag;

    public Boolean getOpenFlag() {
        return openFlag;
    }

    public void setOpenFlag(Boolean openFlag) {
        this.openFlag = openFlag;
    }
}
