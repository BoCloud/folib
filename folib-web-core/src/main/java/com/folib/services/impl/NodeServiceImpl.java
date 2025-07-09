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
package com.folib.services.impl;

import com.folib.components.cassandra.CassandraComponent;
import com.folib.components.node.NodeComponent;
import com.folib.forms.node.CassandraClusterForm;
import com.folib.services.NodeService;
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
