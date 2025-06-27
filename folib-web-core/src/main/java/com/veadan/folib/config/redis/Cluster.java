package com.veadan.folib.config.redis;

import java.util.List;

/**
 * @author veadan
 **/
public class Cluster {

    private List<String> nodeAddresses;

    public List<String> getNodeAddresses() {
        return nodeAddresses;
    }

    public void setNodeAddresses(List<String> nodeAddresses) {
        this.nodeAddresses = nodeAddresses;
    }

    @Override
    public String toString() {
        return "{" +
                "nodeAddresses=" + nodeAddresses +
                '}';
    }
}
