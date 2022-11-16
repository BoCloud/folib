package com.veadan.folib.services.impl;

import com.veadan.folib.components.node.NodeComponent;
import com.veadan.folib.forms.node.CassandraClusterForm;
import com.veadan.folib.services.NodeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author leipenghui
 * @date 2022/11/1
 **/
@Service
public class NodeServiceImpl implements NodeService {

    @Autowired
    private NodeComponent nodeComponent;

    @Override
    public CassandraClusterForm cassandraClusterInfo() {
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
}
