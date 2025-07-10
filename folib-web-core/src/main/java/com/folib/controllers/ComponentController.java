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

import com.folib.forms.component.ArtifactGraphForm;
import com.folib.forms.component.ArtifactStatisticsForm;
import com.folib.forms.component.ComponentTableForm;
import com.folib.forms.vulnerability.AffectedArtifactsForm;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.ComponentService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/component")
@Api(description = "组件管理",tags = "分页组件管理")
public class ComponentController extends BaseController {

    @Inject
    private ComponentService componentService;

    @ApiOperation(value = "查询组件分页列表", response = ComponentTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<ComponentTableForm> page(@RequestParam(name = "page", required = false) Integer page,
                                                        @RequestParam(name = "limit", required = false) Integer limit,
                                                        @RequestParam(name = "name", required = false) String name,
                                                        @RequestParam(name = "groupId", required = false) String groupId,
                                                        @RequestParam(name = "version", required = false) String version,
                                                        @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryComponentPage(page, limit, name, groupId, version, searchKeyword);
    }

    @ApiOperation(value = "查询组件分页列表", response = ComponentTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/pageByArtifact")
    public TableResultResponse<ComponentTableForm> pageByArtifact(@RequestParam(name = "page", required = false) Integer page,
                                                                  @RequestParam(name = "limit", required = false) Integer limit,
                                                                  @RequestParam(name = "artifactPath") String artifactPath,
                                                                  @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryComponentPageByArtifact(page, limit, artifactPath, searchKeyword);
    }

    @ApiOperation(value = "查询组件信息", response = ComponentTableForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/single")
    public ResponseEntity<ComponentTableForm> queryComponentOne(@RequestParam(name = "uuid") String uuid) {
        return ResponseEntity.ok(componentService.queryComponentOne(uuid));
    }

    @ApiOperation(value = "根据组件id分页查询关联制品", response = AffectedArtifactsForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactPage")
    public TableResultResponse<AffectedArtifactsForm> artifactPage(@RequestParam(name = "page", required = false) Integer page,
                                                                   @RequestParam(name = "limit", required = false) Integer limit,
                                                                   @RequestParam(name = "componentUuid") String componentUuid,
                                                                   @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryArtifactByComponentUuid(page, limit, componentUuid, searchKeyword);
    }

    @ApiOperation(value = "根据组件id分页查询关联制品", response = ArtifactGraphForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactGraph")
    public ResponseEntity<ArtifactGraphForm> artifactPage(@RequestParam(name = "componentUuid") String componentUuid) {
        return ResponseEntity.ok(componentService.artifactGraph(componentUuid));
    }

    @ApiOperation(value = "组件关联的制品统计数据", response = ArtifactStatisticsForm.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactStatistics")
    public ResponseEntity<ArtifactStatisticsForm> artifactStatistics(@RequestParam(name = "componentUuid") String componentUuid) {
        return ResponseEntity.ok(componentService.artifactStatistics(componentUuid));
    }
}
