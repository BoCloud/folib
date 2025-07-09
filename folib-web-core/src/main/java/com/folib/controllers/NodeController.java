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
package com.folib.controllers;

import com.folib.forms.node.CassandraClusterForm;
import com.folib.services.NodeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * 集群节点
 *
 * @author veadan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/node")
@Api(description = "节点管理",tags = "节点管理")
public class NodeController extends BaseController {

    @Autowired
    private NodeService nodeService;

    /**
     * 获取cassandra集群信息
     *
     * @return 集群信息
     */
    @ApiOperation(value = "获取cassandra集群信息", response = CassandraClusterForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/cassandraClusterInfo")
    public ResponseEntity<CassandraClusterForm> cassandraClusterInfo() {
        return ResponseEntity.ok(nodeService.cassandraClusterInfo());
    }

    /**
     * 从cassandra集群中移除节点
     *
     * @return 结果
     */
    @ApiOperation(value = "从cassandra集群中移除节点")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = "/removeNode")
    public ResponseEntity<String> removeNode(@RequestParam String token) {
        nodeService.removeNode(token);
        return ResponseEntity.ok("");
    }

    /**
     * 修复cassandra集群
     *
     * @return 结果
     */
    @ApiOperation(value = "修复cassandra集群")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = "/repair")
    public ResponseEntity<String> repair() {
        nodeService.repair();
        return ResponseEntity.ok("");
    }

    /**
     * 修改复制因子
     *
     * @return 结果
     */
    @ApiOperation(value = "修改复制因子")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = "/replicationFactor/{replicationFactor}")
    public ResponseEntity<String> modifyReplicationFactor(@PathVariable("replicationFactor") int replicationFactor) {
        nodeService.modifyReplicationFactor(replicationFactor);
        return ResponseEntity.ok("");
    }

    /**
     * 查询复制因子
     *
     * @return 结果
     */
    @ApiOperation(value = "查询复制因子")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/replicationFactor/{keySpace}")
    public ResponseEntity<String> queryReplicationFactor(@PathVariable("keySpace") String keySpace) {
        return ResponseEntity.ok(nodeService.queryReplicationFactor(keySpace));
    }

}
