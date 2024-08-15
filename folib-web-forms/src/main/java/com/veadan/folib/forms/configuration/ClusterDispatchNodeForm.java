package com.veadan.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 节点分发配置form
 *
 * @author qijianping
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ClusterDispatchNodeForm {
    private String clusterEnName;
    private String clusterCnName;
    private String clusterNodeDesc;
    private String clusterNodeHost;
    private String dispatchType;
    private Boolean isThisCluster = false;
    private String createTime;
    /**
     * 节点传输速率（KB/s）
     */
    private Integer kbps;

    private Boolean autoRegister ;
    private Boolean isSyncPrivilege = false;
}
