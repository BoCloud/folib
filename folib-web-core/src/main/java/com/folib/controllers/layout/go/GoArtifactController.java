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
package com.folib.controllers.layout.go;


import com.folib.controllers.BaseArtifactController;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.GoLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@RestController
@LayoutReqMapping(GoLayoutProvider.ALIAS)
@Slf4j
@Api(description = "go坐标控制器",tags = "go坐标控制器")
public class GoArtifactController extends BaseArtifactController {

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

//
//    @ApiOperation(value = "Used to retrieve an sumdb(not support)")
//    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
//    @ApiResponse(code = 400, message = "An error occurred.")})
//    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"sumdb/{url:.+}"})
    public  ResponseEntity<Object> sumdb(@PathVariable String url)
            throws Exception {
        //校验和数据库服务 暂不支持
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body("not support sumdb");
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
    @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{artifactPath:.+}"})
    public ResponseEntity<Object> download(@RepoMapping Repository repository,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           @PathVariable String artifactPath,
                                           HttpServletRequest request,
                                           HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }
}