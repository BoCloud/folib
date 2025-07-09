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
package com.folib.controllers.adapter.jfrog;

import com.folib.constant.GlobalConstants;
import com.folib.controllers.adapter.jfrog.req.ResolveBatchPathReq;
import com.folib.controllers.adapter.jfrog.req.ResolvePathFiles;
import com.folib.controllers.adapter.jfrog.res.BatchDownloadRes;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.Storage;
import com.folib.util.RepositoryPathUtil;
import io.swagger.annotations.Api;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog下载", tags = "JFrog下载")
public class ArtifactDownController extends JFrogBaseController {

    @PreAuthorize("authenticated")
    @GetMapping(value = "/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity download(@PathVariable("repositoryId") String repositoryId, @RequestHeader HttpHeaders httpHeaders, @PathVariable String artifactPath,
                                   HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound();
        }
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);

        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }

    @PreAuthorize("authenticated")
    @PostMapping(value = "/resolveBatchPath")
    public ResponseEntity<Object> resolveBatchPath(@RequestBody @Validated ResolveBatchPathReq resolveBatchPathReq, HttpServletRequest request, HttpServletResponse response) throws Exception {
        if (Objects.isNull(resolveBatchPathReq) || CollectionUtils.isEmpty(resolveBatchPathReq.getFiles())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Parameter files data error, please check");
        }
        List<ResolvePathFiles> files = resolveBatchPathReq.getFiles();
        String pattern, target, tempTarget, storageId, repositoryId, artifactPath;
        Boolean recursive, flat;
        String[] arr;
        List<BatchDownloadRes> batchDownloadResList = Lists.newArrayList();
        for (ResolvePathFiles resolvePathFiles : files) {
            pattern = resolvePathFiles.getPattern();
            pattern = StringUtils.removeStart(pattern, File.separator);
            pattern = StringUtils.removeEnd(pattern, File.separator);
            arr = pattern.split(File.separator);
            if (arr.length < 3) {
                logger.info("Parameter pattern data error, please check");
                continue;
            }
            storageId = arr[0];
            repositoryId = arr[1];
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                logger.info(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
                continue;
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                logger.info(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
                continue;
            }
            target = resolvePathFiles.getTarget();
            target = StringUtils.removeEnd(target, File.separator);
            recursive = resolvePathFiles.getRecursive();
            flat = resolvePathFiles.getFlat();
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getGlobPaths(rootRepositoryPath.getRepository().getLayout(), rootRepositoryPath, pattern, recursive, null);
            if (CollectionUtils.isEmpty(repositoryPathList)) {
                continue;
            }
            for (RepositoryPath repositoryPath : repositoryPathList) {
                URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
                BatchDownloadRes batchDownloadRes = BatchDownloadRes.builder().url(artifactResource.toString()).build();
                if (Boolean.TRUE.equals(flat)) {
                    tempTarget = target + File.separator + RepositoryFiles.relativizePath(repositoryPath);
                } else {
                    tempTarget = target + File.separator + repositoryPath.getFileName().toString();
                }
                batchDownloadRes.setTarget(tempTarget);
                batchDownloadResList.add(batchDownloadRes);
            }
        }
        return ResponseEntity.ok(batchDownloadResList);
    }

}
