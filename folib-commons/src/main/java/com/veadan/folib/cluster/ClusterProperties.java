package com.veadan.folib.cluster;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Component
@Order
public class ClusterProperties {
    private static final Logger logger = LoggerFactory.getLogger(ClusterProperties.class);

    @Value("${folib.cluster.openflag:false}")
    private Boolean openFlag;

    @Value("${folib.cluster.hostnode}")
    private String hostNode;

    public Boolean getOpenFlag() {
        return openFlag;
    }

    public void setOpenFlag(Boolean openFlag) {
        this.openFlag = openFlag;
    }

    public String getHostNode() {
        return hostNode;
    }

    public void setHostNode(String hostNode) {
        this.hostNode = hostNode;
    }

    public List<String> getHostNodeList() {
        try {
            String nodeArray = getHostNode();
            if (StringUtils.isBlank(nodeArray)) {
                return Collections.emptyList();
            }
            String[] array = nodeArray.split(",");
            return Arrays.asList(array);
        } catch (Exception e) {
            logger.error("get host node list error {}", e.getMessage());
        }
        return Collections.emptyList();
    }
}
