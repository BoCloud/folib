/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
