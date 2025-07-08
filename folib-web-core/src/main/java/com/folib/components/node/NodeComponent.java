package com.folib.components.node;

import com.google.common.collect.Maps;
import com.folib.forms.node.CassandraClusterForm;
import lombok.extern.slf4j.Slf4j;
import org.apache.cassandra.repair.RepairParallelism;
import org.apache.cassandra.repair.messages.RepairOption;
import org.apache.cassandra.tools.NodeProbe;
import org.apache.cassandra.tools.NodeTool;
import org.apache.cassandra.tools.nodetool.SetHostStatWithPort;
import org.apache.cassandra.tools.nodetool.stats.StatsHolder;
import org.apache.cassandra.tools.nodetool.stats.TableStatsHolder;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PropertiesLoaderUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @date 2022/11/1
 **/
@Slf4j
@Component("nodeUtil")
public class NodeComponent {

    @Value("${folib.etc}")
    private String folibEtc;

    private NodeProbe nodeProbe;

    private String keyspace;

    @PostConstruct
    public void initNodeProbe() throws IOException {
        Resource resource = new FileSystemResource(folibEtc + "/conf/janusgraph-cassandra.properties");
        Properties properties = PropertiesLoaderUtils.loadProperties(resource);
        String port = System.getProperty("cassandra.jmx.remote.port");
        keyspace = properties.getProperty("storage.cql.keyspace");
        String username = properties.getProperty("storage.username");
        String password = properties.getProperty("storage.password");
        if (StringUtils.isBlank(port)) {
            port = "7199";
        }
        nodeProbe = new NodeProbe("127.0.0.1", Integer.parseInt(port), username, password);
    }

    /**
     * 获取集群信息
     *
     * @return 集群信息
     */
    public CassandraClusterForm cassandraClusterInfo() {
        CassandraClusterForm cassandraClusterForm = CassandraClusterForm.builder().build();
        try {
            //本地主机id
            String localHostId = nodeProbe.getLocalHostId();
            cassandraClusterForm.setLocalHostId(localHostId);
            //主机id map
            Map<String, String> hostIDMap = nodeProbe.getHostIdMap(true);
            cassandraClusterForm.setHostIDMap(hostIDMap);
            //本地ip
            String endpoint = hostIDMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey)).get(localHostId);
            cassandraClusterForm.setEndpoint(endpoint);
            //load map
            Map<String, String> loadMap = nodeProbe.getLoadMap(true);
            cassandraClusterForm.setLoadMap(loadMap);
            //token信息
            List<String> tokenList = nodeProbe.getTokens();
            cassandraClusterForm.setTokenList(tokenList);
            //无法访问的节点信息
            List<String> unreachableNodeList = nodeProbe.getUnreachableNodes(true);
            cassandraClusterForm.setUnreachableNodeList(unreachableNodeList);
            //加入的节点信息
            List<String> joiningNodeList = nodeProbe.getJoiningNodes(true);
            cassandraClusterForm.setJoiningNodeList(joiningNodeList);
            //正常的节点信息
            List<String> liveNodeList = nodeProbe.getLiveNodes(true);
            cassandraClusterForm.setLiveNodeList(liveNodeList);
            //移动的节点信息
            List<String> movingNodeList = nodeProbe.getMovingNodes(true);
            cassandraClusterForm.setMovingNodeList(movingNodeList);
            //离开的节点信息
            List<String> leavingNodeList = nodeProbe.getLeavingNodes(true);
            cassandraClusterForm.setLeavingNodeList(leavingNodeList);
            //获取所有权
            Map<String, Float> ownershipMap = nodeProbe.getOwnershipWithPort();
            cassandraClusterForm.setOwnershipMap(ownershipMap);
            //有效所有权
            Map<String, Float> effectiveOwnershipMap = nodeProbe.effectiveOwnershipWithPort(keyspace);
            cassandraClusterForm.setEffectiveOwnershipMap(effectiveOwnershipMap);
            //种子节点信息
            List<String> seedList = nodeProbe.getSeeds();
            cassandraClusterForm.setSeedList(seedList);
            //拓扑信息
            String gossipInfo = nodeProbe.getGossipInfo(true);
            cassandraClusterForm.setGossipInfo(gossipInfo);
            //数据信息
            StatsHolder statsHolder = new TableStatsHolder(nodeProbe, true, false, Collections.emptyList(), "", 0, false);
            Map<String, Object> statsHolderMap = statsHolder.convert2Map();
            cassandraClusterForm.setStatsHolderMap(statsHolderMap);
            //dcsMap
            Map<String, String> tokensToEndpointsMap = nodeProbe.getTokenToEndpointMap(true);
            cassandraClusterForm.setTokensToEndpointsMap(tokensToEndpointsMap);
            SortedMap<String, SetHostStatWithPort> dcsMap = NodeTool.getOwnershipByDcWithPort(nodeProbe, true, tokensToEndpointsMap, effectiveOwnershipMap);
            cassandraClusterForm.setDcsMap(dcsMap);
        } catch (Exception ex) {
            log.error("获取cassandra集群信息错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("获取集群信息错误，请稍候重试");
        }
        return cassandraClusterForm;
    }

    /**
     * 获取集群节点个数
     *
     * @return 集群节点个数
     */
    public int getClusterNodeTotal() {
        Map<String, Float> effectiveOwnershipMap = nodeProbe.effectiveOwnershipWithPort(keyspace);
        Map<String, String> tokensToEndpointsMap = nodeProbe.getTokenToEndpointMap(true);
        SortedMap<String, SetHostStatWithPort> dcsMap = NodeTool.getOwnershipByDcWithPort(nodeProbe, true, tokensToEndpointsMap, effectiveOwnershipMap);
        int nodeTotal = 0;
        for (Map.Entry<String, SetHostStatWithPort> entry : dcsMap.entrySet()) {
            nodeTotal = nodeTotal + entry.getValue().size();
        }
        return nodeTotal;
    }

    /**
     * 移除节点
     *
     * @param token token
     */
    public void removeNode(String token) {
        log.info("从cassandra集群移除节点：{}", token);
        String localHostId = nodeProbe.getLocalHostId();
        if (localHostId.equals(token)) {
            throw new UnsupportedOperationException("不能移除自身");
        }
        nodeProbe.removeNode(token);
        log.info("从cassandra集群移除节点：{} 完成", token);
    }

    /**
     * 修复
     */
    public void repair() {
        try {
            log.info("开始尝试修复cassandra集群信息");
            Map<String, String> options = Maps.newLinkedHashMap();
            options.put(RepairOption.PARALLELISM_KEY, RepairParallelism.DATACENTER_AWARE.getName());
            nodeProbe.repairAsync(System.out, keyspace, options);
            log.info("尝试修复cassandra集群信息结束");
        } catch (IOException ex) {
            log.error("修复cassandra集群错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("修复错误，请稍候重试");
        }
    }

}
