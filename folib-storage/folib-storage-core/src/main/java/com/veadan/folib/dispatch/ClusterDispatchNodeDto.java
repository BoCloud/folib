package com.veadan.folib.dispatch;

import com.fasterxml.jackson.annotation.JsonRootName;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 制品节点分发实体
 *
 * @author qijianping
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonRootName("clusterDispatchNode")
public class ClusterDispatchNodeDto implements Serializable {
    /**
     * 集群英文名
     */
    private String clusterEnName;

    /**
     * 集群中文名
     */
    private String clusterCnName;

    /**
     * 节点描述
     */
    private String clusterNodeDesc;

    /**
     * 节点访问url地址
     */
    private String clusterNodeHost;

    /**
     * pull push 分发类型
     */
    private String dispatchType;

    /**
     * 是否本节点
     */
    private Boolean isThisCluster;

    /**
     * 创建时间
     */
    private String createTime;

    /**
     * 自动注册<br>
     * 自动注册的节点无法编辑或删除
     */
    private Boolean autoRegister = false;

    /**
     * Ws客户端节点在线状态
     */
    private Boolean wsClientOnline;


    /**
     * 节点传输速率（Kbs）
     */
    private Long kbps;
}
