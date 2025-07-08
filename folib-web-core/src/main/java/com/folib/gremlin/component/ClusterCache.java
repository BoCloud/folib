package com.folib.gremlin.component;

import com.folib.gremlin.util.GremlinUtil;
import org.apache.tinkerpop.gremlin.driver.Client;
import org.apache.tinkerpop.gremlin.driver.Cluster;
import org.janusgraph.graphdb.tinkerpop.JanusGraphIoRegistry;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @Author: haifeng
 * @Date: 2019-09-10 10:50
 */
@Component
public class ClusterCache {

    private Map<String, Cache> cacheMap = new HashMap<>(5);

    private String key(String host, int port) {
        return String.format("%s:%s", host, port);
    }

    public Client get(String host, int port) {
        Cache cache = cacheMap.get(key(host, port));
        return cache == null ? null : cache.client;
    }

    public Client put(String host, int port) {
        String key = key(host, port);
        Cache cache = cacheMap.get(key);
        if (cache == null) {
            Cluster cluster = GremlinUtil.cluster(host, port, JanusGraphIoRegistry.instance());
            Client client = cluster.connect().init();
            Date create = new Date();
            cacheMap.put(key, new Cache(cluster, client, create.getTime()));
            return client;
        } else {
            return cache.getClient();
        }
    }


    public void clear(int second) {
        for (String key : cacheMap.keySet()) {
            Cache cache = cacheMap.get(key);
            Date date = new Date();
            if ((date.getTime() - cache.getCreateTime()) / 1000 > second) {
                try {
                    cache.getClient().close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
                try {
                    cache.getCluster().close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
                cacheMap.remove(key);
            }
        }
    }


    private static class Cache {
        private Cluster cluster;
        private Client client;
        private long createTime;

        public Cache(Cluster cluster, Client client, long createTime) {
            this.cluster = cluster;
            this.client = client;
            this.createTime = createTime;
        }

        public Cluster getCluster() {
            return cluster;
        }

        public void setCluster(Cluster cluster) {
            this.cluster = cluster;
        }

        public Client getClient() {
            return client;
        }

        public void setClient(Client client) {
            this.client = client;
        }

        public long getCreateTime() {
            return createTime;
        }

        public void setCreateTime(long createTime) {
            this.createTime = createTime;
        }
    }

}
