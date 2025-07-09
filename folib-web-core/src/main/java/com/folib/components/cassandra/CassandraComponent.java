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
package com.folib.components.cassandra;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;
import com.datastax.driver.core.Session;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PropertiesLoaderUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.util.Objects;
import java.util.Properties;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class CassandraComponent {

    private static String KEYS_PACE_NAME = "folib";
    private static final String SYSTEM_AUTH_KEYS_PACE_NAME = "system_auth";
    private static String NODE_IP_ADDRESS = "127.0.0.1";
    private static String NODE_PORT = "49142";
    private static String USERNAME = "root";
    private static String PASSWORD = "super@V587";
    private static Cluster CLUSTER;
    private static Session SESSION;
    public static final String GC_GRACE_SECONDS_KEY = "GC_GRACE_SECONDS";
    public static final Integer DEFAULT_GC_GRACE_SECONDS = 864000;

    @Value("${folib.etc}")
    private String folibEtc;

    @PostConstruct
    public void initNodeProbe() throws IOException {
        Resource resource = new FileSystemResource(folibEtc + "/conf/janusgraph-cassandra.properties");
        Properties properties = PropertiesLoaderUtils.loadProperties(resource);
        NODE_IP_ADDRESS = System.getProperty("EMBEDDED_DB_HOST", NODE_IP_ADDRESS);
        NODE_PORT = properties.getProperty("storage.port", NODE_PORT);
        KEYS_PACE_NAME = properties.getProperty("storage.cql.keyspace", KEYS_PACE_NAME);
        USERNAME = properties.getProperty("storage.username", USERNAME);
        PASSWORD = properties.getProperty("storage.password", PASSWORD);
    }

    /**
     * 初始化实例
     */
    private void initCluster() {
        if (Objects.isNull(CLUSTER) || CLUSTER.isClosed()) {
            CLUSTER = Cluster.builder()
                    .addContactPoints(NODE_IP_ADDRESS)
                    .withCredentials(USERNAME, PASSWORD)
                    .withPort(Integer.parseInt(NODE_PORT))
                    .build();
        }
    }

    /**
     * 关闭实例
     */
    private void closeCluster() {
        if (Objects.nonNull(CLUSTER)) {
            CLUSTER.close();
        }
    }

    /**
     * 打开session
     */
    private void openSession() {
        if (Objects.isNull(SESSION) || SESSION.isClosed()) {
            SESSION = CLUSTER.connect();
        }
    }

    /**
     * 关闭session
     */
    private void closeSession() {
        if (Objects.nonNull(SESSION)) {
            SESSION.close();
        }
    }

    private void open() {
        initCluster();
        openSession();
    }

    private void close() {
        closeSession();
        closeCluster();
    }

    /**
     * 修改复制因子
     *
     * @param replicationFactor 复制因子
     */
    public void modifyReplicationFactor(int replicationFactor) {
        if (replicationFactor < 1) {
            return;
        }
        try {
            open();

            print();

            String query = String.format("ALTER KEYSPACE  %s WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1' : %s}", KEYS_PACE_NAME, replicationFactor);
            SESSION.execute(query);
            log.info("{} replication factor updated to {}", KEYS_PACE_NAME, replicationFactor);

            query = String.format("ALTER KEYSPACE  %s WITH REPLICATION = {'class' : 'NetworkTopologyStrategy', 'datacenter1' : %s}", SYSTEM_AUTH_KEYS_PACE_NAME, replicationFactor);
            SESSION.execute(query);
            log.info("{} replication factor updated to {}", SYSTEM_AUTH_KEYS_PACE_NAME, replicationFactor);

            print();

        } finally {
            close();
        }
    }

    /**
     * 打印当前复制因子信息
     */
    private static void print() {
        String query = String.format("SELECT * FROM system_schema.keyspaces WHERE keyspace_name = %s%s%s", "'", KEYS_PACE_NAME, "'");
        ResultSet rs = SESSION.execute(query);
        Row row = rs.one();
        log.info("{} current info: {}", KEYS_PACE_NAME, row);

        query = String.format("SELECT * FROM system_schema.keyspaces WHERE keyspace_name = %s%s%s", "'", SYSTEM_AUTH_KEYS_PACE_NAME, "'");
        rs = SESSION.execute(query);
        row = rs.one();
        log.info("{} current info: {}", SYSTEM_AUTH_KEYS_PACE_NAME, row);
    }

    /**
     * 查询复制因子
     *
     * @param keySpace keySpace
     * @return 复制因子信息
     */
    public String queryReplicationFactor(String keySpace) {
        try {
            open();

            String query = String.format("SELECT * FROM system_schema.keyspaces WHERE keyspace_name = %s%s%s", "'", keySpace, "'");
            ResultSet rs = SESSION.execute(query);
            if (Objects.nonNull(rs)) {
                Row row = rs.one();
                if (Objects.nonNull(row)) {
                    return row.toString();
                }
            }
            return "Not found";
        } finally {
            close();
        }
    }

    public void modifyGcGraceSeconds(Integer seconds) {
        if (Objects.isNull(seconds) || seconds < 0) {
            return;
        }
        try {
            open();

            printGc();

            String query = String.format("ALTER TABLE %s.edgestore WITH GC_GRACE_SECONDS = %s", KEYS_PACE_NAME, seconds);
            SESSION.execute(query);
            log.info("{} GC grace seconds updated to {}", KEYS_PACE_NAME, seconds);

            printGc();

        } finally {
            close();
        }
    }

    /**
     * 打印GC
     */
    private static void printGc() {
        String query = String.format("SELECT table_name,gc_grace_seconds FROM system_schema.tables WHERE keyspace_name = %s%s%s", "'", KEYS_PACE_NAME, "'");
        ResultSet rs = SESSION.execute(query);
        Row row = rs.one();
        log.info("{} GC grace seconds current info: {}", KEYS_PACE_NAME, row);
    }
}
