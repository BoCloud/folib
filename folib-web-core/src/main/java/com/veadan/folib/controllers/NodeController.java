package com.veadan.folib.controllers;

import com.veadan.folib.forms.node.CassandraClusterForm;
import com.veadan.folib.services.NodeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * 集群节点
 *
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/node")
@Api(value = "/api/node")
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

}
