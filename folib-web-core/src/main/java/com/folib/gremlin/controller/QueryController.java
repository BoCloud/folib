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
package com.folib.gremlin.controller;

import com.folib.gremlin.entity.QueryResult;
import com.folib.gremlin.entity.vo.PropertyVo;
import com.folib.gremlin.service.QueryService;
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
