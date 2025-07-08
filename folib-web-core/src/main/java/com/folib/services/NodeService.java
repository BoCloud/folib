package com.folib.services;

import com.folib.forms.node.CassandraClusterForm;

/**
 * @author veadan
 * @date 2022/11/1
 **/
public interface NodeService {

    /**
     * 获取集群信息
     *
     * @return 集群信息
     */
    CassandraClusterForm cassandraClusterInfo();

    /**
     * 移除节点
     *
     * @param token token
     */
    void removeNode(String token);

    /**
     * 修复节点
     */
    void repair();

    /**
     * 修改复制因子
     *
     * @param replicationFactor 复制因子
     */
    void modifyReplicationFactor(int replicationFactor);

    /**
     * 查询复制因子
     *
     * @param keySpace keySpace
     * @return 复制因子信息
     */
    String queryReplicationFactor(String keySpace);

    /**
     * 修改gcGraceSeconds
     * @param seconds 秒
     */
    void modifyGcGraceSeconds(Integer seconds);
}
