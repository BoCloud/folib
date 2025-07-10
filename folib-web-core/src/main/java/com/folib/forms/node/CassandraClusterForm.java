package com.folib.forms.node;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.cassandra.tools.nodetool.SetHostStatWithPort;

import java.util.List;
import java.util.Map;
import java.util.SortedMap;

/**
 * @author leipenghui
 * @date 2022/11/1
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CassandraClusterForm {

    /**
     * 本机ip
     */
    private String endpoint;
    /**
     * 本地主机id
     */
    private String localHostId;
    /**
     * 主机id map
     */
    private Map<String, String> hostIDMap;
    /**
     * load map
     */
    private Map<String, String> loadMap;
    /**
     * token信息
     */
    private List<String> tokenList;
    /**
     * 无法访问的节点信息
     */
    private List<String> unreachableNodeList;
    /**
     * 加入的节点信息
     */
    private List<String> joiningNodeList;
    /**
     * 正常的节点信息
     */
    private List<String> liveNodeList;
    /**
     * 移动的节点信息
     */
    private List<String> movingNodeList;
    /**
     * 离开的节点信息
     */
    private List<String> leavingNodeList;
    /**
     * 获取所有权
     */
    private Map<String, Float> ownershipMap;
    /**
     * 有效所有权
     */
    private Map<String, Float> effectiveOwnershipMap;
    /**
     * 种子节点信息
     */
    private List<String> seedList;
    /**
     * 拓扑信息
     */
    private String gossipInfo;
    /**
     * 数据信息
     */
    private Map<String, Object> statsHolderMap;
    /**
     * doc map
     */
    private SortedMap<String, SetHostStatWithPort> dcsMap;
    /**
     * token和ip信息
     */
    private Map<String, String> tokensToEndpointsMap;
}
