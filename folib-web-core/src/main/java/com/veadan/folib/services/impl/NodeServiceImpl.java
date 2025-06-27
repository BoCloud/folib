package com.veadan.folib.services.impl;

import com.veadan.folib.components.cassandra.CassandraComponent;
import com.veadan.folib.components.node.NodeComponent;
import com.veadan.folib.dto.node.CassandraClusterDto;
import com.veadan.folib.services.NodeService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author veadan
 * @date 2022/11/1
 **/
@Slf4j
@Service
public class NodeServiceImpl implements NodeService {

    @Autowired
    private NodeComponent nodeComponent;

    @Autowired
    private CassandraComponent cassandraComponent;

    @Override
    public CassandraClusterDto cassandraClusterInfo() {
        return nodeComponent.cassandraClusterInfo();
    }

    @Override
    public void removeNode(String token) {
        nodeComponent.removeNode(token);
    }

    @Override
    public void repair() {
        nodeComponent.repair();
    }

    @Override
    public void modifyReplicationFactor(int replicationFactor) {
        try {
            cassandraComponent.modifyReplicationFactor(replicationFactor);
        } catch (Exception ex) {
            log.error("Modify replication factor error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public String queryReplicationFactor(String keySpace) {
        return cassandraComponent.queryReplicationFactor(keySpace);
    }

    @Override
    public void modifyGcGraceSeconds(Integer seconds) {
        try {
            cassandraComponent.modifyGcGraceSeconds(seconds);
        } catch (Exception ex) {
            log.error("Modify gc grace seconds error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }
}
