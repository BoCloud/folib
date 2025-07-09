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
package com.folib.controllers.layout.debian;


import com.folib.constant.DebianConstant;
import com.folib.controllers.BaseArtifactController;
import com.folib.providers.io.RepositoryPath;
import com.folib.web.LayoutReqMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * @author veadan
 * @since 2024-09-06 10:36
 */
@RestController
@LayoutReqMapping(DebianConstant.LAYOUT_NAME)
@Api(tags = "debian 坐标控制器")
public class DebianArtifactController extends BaseArtifactController {

    @Override
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(
            @RequestHeader HttpHeaders httpHeaders,
            @PathVariable String artifactPath,
            @PathVariable String storageId,
            @PathVariable String repositoryId,
            HttpServletRequest request,
            HttpServletResponse response)
            throws Exception {
        long startTime = System.currentTimeMillis();
        logger.info("Requested /{}/{}/{}", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        logger.debug("Requested /{}/{}/{} endTime {} .", storageId, repositoryId, artifactPath, System.currentTimeMillis() - startTime);
    }

}
