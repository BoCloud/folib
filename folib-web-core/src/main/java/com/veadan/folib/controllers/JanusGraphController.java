package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSONObject;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.janusgraph.core.JanusGraph;
import org.janusgraph.core.schema.JanusGraphManagement;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Set;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/janusGraph")
@Api(value = "/api/janusGraph")
public class JanusGraphController extends BaseController {

    @Inject
    private JanusGraph janusGraph;

    @ApiOperation(value = "删除指定实例")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping(value = "/instance/{instanceId}")
    public void deleteInstance(@PathVariable(name = "instanceId") String instanceId) {
        String current = "(current)";
        if (instanceId.contains(current)) {
            return;
        }
        JanusGraphManagement janusGraphManagement = janusGraph.openManagement();
        try {
            Set<String> ids = janusGraphManagement.getOpenInstances();
            log.info("=====>>>>> 所有实例：{}，要删除的实例：{}", ids, instanceId);
            janusGraphManagement.forceCloseInstance(instanceId);
            janusGraphManagement.commit();
        } catch (Exception ex) {
            log.error("=====>>>>> 删除实例异常：", ex);
            janusGraphManagement.rollback();
            throw new RuntimeException(ex);
        }
    }

    @ApiOperation(value = "查询janusGraph信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping
    public ResponseEntity<JSONObject> janusGraphInfo() {
        JSONObject data = new JSONObject();
        JanusGraphManagement janusGraphManagement = janusGraph.openManagement();
        try {
            data.put("openInstances", janusGraphManagement.getOpenInstances());
            data.put("schema", janusGraphManagement.printSchema());
            janusGraphManagement.rollback();
        } catch (Exception ex) {
            log.error("=====>>>>> 查询janusGraph信息异常：", ex);
            janusGraphManagement.rollback();
            throw new RuntimeException(ex);
        }
        return ResponseEntity.ok(data);
    }
}
