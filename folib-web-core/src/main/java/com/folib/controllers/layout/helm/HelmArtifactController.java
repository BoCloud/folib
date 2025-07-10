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
package com.folib.controllers.layout.helm;

import com.folib.config.HelmRepoUtil;
import com.folib.controllers.BaseArtifactController;
import com.folib.indexer.HelmMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.*;
import java.nio.file.Files;
import java.util.Objects;


/**
 * Helm 布局逻辑控制层
 *
 * @author veadan
 */
@LayoutReqMapping("helm")
@RestController
@Api(description = "Helm坐标控制器",tags = "Helm坐标控制器")
public class HelmArtifactController extends BaseArtifactController {

    @Autowired
    private HelmRepoUtil helmRepoUtil;

    @Autowired
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/{storageId}/{repositoryId}/{path:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void download(@RepoMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception {
        try {
            final String storageId = repository.getStorage().getId();
            final String repositoryId = repository.getId();
            logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
            if (null != repositoryPath && repository.getType().equalsIgnoreCase("proxy")
                    && path.equals("index.yaml") && repository.getLayout().equalsIgnoreCase("helm")) {
                if (Files.exists(repositoryPath)) {
                    proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
                }
            }

            if (null == repositoryPath && repository.getType().equals("hosted") && path.equals("index.yaml")) {
                repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
                if (!Files.exists(repositoryPath)) {
                    // 创建刷新索引
                    //RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "");
                    //String absolutePath = repoPath.toAbsolutePath().toString();
                    //helmRepoUtil.createIndex(absolutePath, repository);
                    HelmMetadataIndexer indexer = new HelmMetadataIndexer(storageId, repositoryId, artifactManagementService, repositoryPathResolver);
                    indexer.reindexAsSystem();
                }
                try (InputStream in = Files.newInputStream(repositoryPath);) {
                    OutputStream out = response.getOutputStream();
                    response.setCharacterEncoding("UTF-8");
                    // 设置文件头：设置下载文件名
                    response.setHeader("Content-Disposition", "attachment;" + " filename=index.yaml");
                    int byteRead = 0;
                    byte[] buffer = new byte[8192];
                    while ((byteRead = in.read(buffer)) != -1) {
                        out.write(buffer, 0, byteRead);
                    }
                    out.flush();
                    return;
                }
            }
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("download helm artifact error {}", e.getMessage());
        }

    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"{storageId}/{repositoryId}/charts","/{storageId}/{repositoryId}/api/charts"}, method = {RequestMethod.POST})
    public ResponseEntity upload(@RepoMapping Repository repository,
                                 @RequestHeader HttpHeaders httpHeaders,
                                 HttpServletRequest request,
                                 HttpServletResponse response,
                                 @RequestParam("chart") MultipartFile[] charts) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            for (MultipartFile file : charts) {
                String[] strArray = file.getOriginalFilename().split("/");
                String fileName = strArray[strArray.length - 1];
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, fileName);
                artifactManagementService.store(repositoryPath, file.getInputStream());
            }
            // 创建刷新索引
            RepositoryPath repoPath = repositoryPathResolver.resolve(repository, "");
            String absolutePath = repoPath.toAbsolutePath().toString();
            //helmRepoUtil.createIndex(absolutePath, repository);
            HelmMetadataIndexer indexer = new HelmMetadataIndexer(storageId, repositoryId, artifactManagementService, repositoryPathResolver);
            indexer.reindexAsSystem();
            return ResponseEntity.status(HttpStatus.CREATED).build();
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }


    @ApiOperation(value = "download  index.yml")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value ="{storageId}/{repositoryId}/index.yaml", method = {RequestMethod.GET})
    public void downloadIndex(@RepoMapping Repository repository,
                              @RequestHeader HttpHeaders httpHeaders,
                              HttpServletRequest request,
                              HttpServletResponse response) {

        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        try {
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, "index.yaml");
            if (Objects.isNull(repositoryPath) && RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                // 创建刷新索引
                //helmRepoUtil.createIndex(absolutePath, repository);
                HelmMetadataIndexer indexer = new HelmMetadataIndexer(storageId, repositoryId, artifactManagementService, repositoryPathResolver);
                indexer.reindexAsSystem();
                repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, "index.yaml");
            }
            if(repositoryPath == null && RepositoryTypeEnum.PROXY.getType().equals(repository.getType())){
                repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, "charts/index.yaml");
            }
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        } catch (Exception e) {
            logger.error("download helm index.yml artifact error {}", e.getMessage());
            throw new RuntimeException(e);
        }
    }


}
