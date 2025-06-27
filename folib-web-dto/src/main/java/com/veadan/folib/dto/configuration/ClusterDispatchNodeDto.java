package com.veadan.folib.dto.configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 节点分发配置form
 *
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ClusterDispatchNodeDto {
    private String clusterEnName;
    private String clusterCnName;
    private String clusterNodeDesc;
    private String clusterNodeHost;
    private String dispatchType = "push";
    private Boolean isThisCluster = false;
    private String createTime;
    /**
     * 节点传输速率（KB/s）
     */
    private Integer kbps;

    private Boolean autoRegister ;
    private Boolean isSyncPrivilege = false;
    /**
     * 同步策略
     */
    private String syncStrategy;
}
