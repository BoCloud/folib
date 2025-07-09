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

import com.folib.providers.search.SearchException;
import com.folib.services.ArtifactSearchService;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResults;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.net.URLDecoder;

import io.swagger.annotations.*;
import org.apache.lucene.queryparser.classic.ParseException;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author 
 */
@Controller
@RequestMapping("/api/search")
@Api(description = "搜索组件控制器",tags = "搜索组件控制器")
public class SearchController
        extends BaseController
{

    @Inject
    ArtifactSearchService artifactSearchService;

    /**
     * Performs a search against the Lucene index of a specified repository,
     * or the Lucene indexes of all repositories.
     *
     * @param storageId
     * @param repositoryId
     * @param query
     * @return
     * @throws IOException
     * @throws ParseException
     */
    @ApiOperation(value = "Used to search for artifacts.", response = SearchResults.class)
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @PreAuthorize("hasAuthority('SEARCH_ARTIFACTS')")
    @GetMapping(consumes = { MediaType.APPLICATION_OCTET_STREAM_VALUE,
                             MediaType.TEXT_PLAIN_VALUE },
                produces = { MediaType.APPLICATION_JSON_VALUE,
                             MediaType.TEXT_PLAIN_VALUE })
    public ResponseEntity search(@ApiParam(value = "The storageId", required = false)
                                 @RequestParam(name = "storageId", required = false) final String storageId,
                                 @ApiParam(value = "The repositoryId", required = false)
                                 @RequestParam(name = "repositoryId", required = false) final String repositoryId,
                                 @ApiParam(value = "The search query", required = true)
                                 @RequestParam(name = "q") final String query,
                                 HttpServletRequest request)
            throws IOException, SearchException
    {
        String accept = request.getHeader("accept");
        String q = URLDecoder.decode(query, "UTF-8");

        logger.info("[search] {}\n\taccept {}\n\tstorageId = {}\n\trepositoryId = {}",
                     q, accept, storageId, repositoryId);

        if (accept.equalsIgnoreCase(MediaType.TEXT_PLAIN_VALUE))
        {
            final SearchResults artifacts = getSearchResults(storageId, repositoryId, q);

            return ResponseEntity.ok(artifacts.toString());
        }
        else
        {
            // Apparently, the JSON root tag's name is based on the name of the object
            // which the Jersey method returns, hence this is "artifacts".
            @SuppressWarnings("UnnecessaryLocalVariable")
            final SearchResults artifacts = getSearchResults(storageId, repositoryId, q);

            return ResponseEntity.ok(artifacts);
        }
    }

    private SearchResults getSearchResults(String storageId,
                                           String repositoryId,
                                           String query)
            throws SearchException
    {
        return artifactSearchService.search(new SearchRequest(storageId,
                                                              repositoryId,
                                                              query));
    }

}
