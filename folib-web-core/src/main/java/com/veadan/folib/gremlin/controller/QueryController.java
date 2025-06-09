package com.veadan.folib.gremlin.controller;

import com.veadan.folib.gremlin.entity.QueryResult;
import com.veadan.folib.gremlin.entity.vo.PropertyVo;
import com.veadan.folib.gremlin.service.QueryService;
import io.swagger.annotations.Api;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * @Author: haifeng
 * @Date: 2019-08-29 11:12
 */
@RestController
@CrossOrigin(origins = "*", maxAge = 3600)
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/gremlin")
@Api(description = "gremlin处理模块",tags = "gremlin处理模块")
public class QueryController {

    private final QueryService queryService;

    public QueryController(QueryService queryService) {
        this.queryService = queryService;
    }

    private String gremlinHost="localhost";
    private int gremlinPort=8182;

    @RequestMapping("/query")
    public QueryResult query(String sourceName, String gremlin) {
        return queryService.query(gremlinHost, gremlinPort, gremlin, sourceName);
    }

    @RequestMapping("/vertex")
    public PropertyVo queryVertex(String sourceName, String id) {
        return queryService.getValueMap(gremlinHost, gremlinPort, sourceName, id, true);
    }

    @RequestMapping("/edge")
    public PropertyVo queryEdge( String sourceName, String id) {
        return queryService.getValueMap(gremlinHost, gremlinPort, sourceName, id, false);
    }

    @GetMapping("/artifacts")
    public ResponseEntity<?> queryArtifacts(@RequestParam (value = "pageNum", defaultValue = "1") int pageNum,
                                         @RequestParam (value = "pageSize", defaultValue = "10") int pageSize,
                                         @RequestParam(value = "artifactSize", defaultValue = "0") long artifactSize
    ) {
        // 默认单位MB
        long size = artifactSize  == 0 ? 0 : artifactSize * 8388608;
        return ResponseEntity.ok(queryService.queryArtifacts(gremlinHost, gremlinPort, pageNum, pageSize, size));
    }

}
