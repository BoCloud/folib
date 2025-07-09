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
package com.folib.controllers.aql;

import com.folib.controllers.BaseController;
import com.folib.services.impl.FqlSearchService;
import com.folib.storage.search.SearchResults;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;

/**
 * @author veadan
 */
@Controller
@RequestMapping("/api/fql")
@Api(description = "aql管理控制器", tags = "aql管理控制器")
public class AqlController extends BaseController {

    @Inject
    private FqlSearchService fqlSearchService;

    @ApiOperation(value = "Used to search for artifacts.", response = SearchResults.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('SEARCH_ARTIFACTS')")
    @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity search(@RequestParam(name = "artifactName", required = false) String artifactName,
                                 @RequestParam(name = "metadataSearch", required = false) String metadataSearch,
                                 @RequestParam(name = "regex", required = false) Boolean regex,
                                 @RequestParam(name = "storageId", required = false) String storageId,
                                 @RequestParam(name = "repositoryId", required = false) String repositoryId,
                                 @RequestParam(name = "beginDate", required = false) String beginDate,
                                 @RequestParam(name = "endDate", required = false) String endDate,
                                 @RequestParam(name = "sortField", required = false) String sortField,
                                 @RequestParam(name = "sortOrder", required = false) String sortOrder,
                                 @RequestParam(name = "repositoryIds", required = false) List<String> repositoryIds,
                                 @RequestParam(name = "safeLevel", required = false) String safeLevel,
                                 @RequestParam(name = "digestAlgorithm", required = false) String digestAlgorithm,
                                 @RequestParam(name = "digest", required = false) String digest,
                                 @RequestParam(name = "query", required = false) String query,
                                 @RequestParam(name = "limit", required = false) Integer limit,
                                 @RequestParam(name = "page", required = false) Integer page) throws IOException {
        SearchResults result = fqlSearchService.artifactQuery(regex, artifactName, metadataSearch, storageId, repositoryId, beginDate, endDate, sortField, sortOrder, repositoryIds, safeLevel, digestAlgorithm, digest, query, limit, page);
        return ResponseEntity.ok(result);
    }

}
