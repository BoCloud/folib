package com.veadan.folib.services;

import com.veadan.folib.forms.node.CassandraClusterForm;

/**
 * @author leipenghui
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
     * @param token token
     */
    void removeNode(String token);

    /**
     * 修复节点
     */
    void repair();
}
