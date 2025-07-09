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
package com.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import com.folib.artifact.coordinates.CocoapodsCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.domain.ArtifactEntity;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.service.CocoapodsIndexService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.CocoapodsArtifactUtil;
import com.folib.util.CompressUtil;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2023/8/3 15:29
 */
@RestController
@LayoutReqMapping(CocoapodsCoordinates.LAYOUT_NAME)
public class CocoapodsArtifactController extends BaseArtifactController
{
    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private CocoapodsIndexService cocoapodsIndexService;

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity uploadPod(@RepoMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try {
            final BufferedInputStream artifactByteArrayInputStream = new BufferedInputStream(request.getInputStream());
            final byte[] cacheBytes = artifactByteArrayInputStream.readAllBytes();
            // 兼容网络路径
            final String podspecSourceContent = CocoapodsArtifactUtil.fetchPodspecSourceContentByInputStream(new ByteArrayInputStream(cacheBytes));

            // 读取pod.tar.gz中的*.podspec文件内容
            final RepositoryPath podRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (StringUtils.isNotBlank(podspecSourceContent))
            {
                final CocoapodsArtifactUtil.PodSpec podSpec = CocoapodsArtifactUtil.resolvePodSpec(podspecSourceContent);
                // 存储制品文件
                final ArtifactEntity podArtifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(podRepositoryPath));
                final CocoapodsCoordinates podArtifactCoordinates = (CocoapodsCoordinates) podArtifactEntity.getArtifactCoordinates();
                podArtifactCoordinates.setBaseName(podSpec.getName());
                podArtifactCoordinates.setVersion(podSpec.getVersion());
                podRepositoryPath.setArtifact(podArtifactEntity);

                
                // 存储索引文件
                try (final ByteArrayInputStream podspecContentByteArrayInputStream = new ByteArrayInputStream(podspecSourceContent.getBytes(StandardCharsets.UTF_8)); ByteArrayInputStream inputStream =new ByteArrayInputStream(cacheBytes)) {
                    artifactManagementService.validateAndStore(podRepositoryPath, inputStream);
                    final String uri = podRepositoryPath.toUri().getPath();
                    final RepositoryPath podSpecRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, String.format(".specs/%s/%s/%s.podspec", podSpec.getName(), podSpec.getVersion(), podSpec.getName()));
                    final ArtifactEntity podSpecArtifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(podSpecRepositoryPath));
                    final CocoapodsCoordinates artifactCoordinates = (CocoapodsCoordinates) podSpecArtifactEntity.getArtifactCoordinates();
                    artifactCoordinates.setPath(uri);
                    artifactCoordinates.setBaseName(podSpec.getName());
                    artifactCoordinates.setVersion(podSpec.getVersion());
                    podSpecRepositoryPath.setArtifact(podSpecArtifactEntity);
                    artifactManagementService.validateAndStore(podSpecRepositoryPath, podspecContentByteArrayInputStream);
                }

            }
            else
            {
                try(ByteArrayInputStream  is = new ByteArrayInputStream(cacheBytes)){
                    artifactManagementService.validateAndStore(podRepositoryPath, is);
                }
            }

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = { "{storageId}/{repositoryId}/{path:.+}" })
    public void download(@RepoMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable String path,
                         HttpServletRequest request,
                         HttpServletResponse response)
            throws Exception
    {
        final String type = repository.getType();
        if (type.equals(RepositoryTypeEnum.HOSTED.getType()))
        { this.downloadHosted(repository, httpHeaders, path, request, response); }
        else if (type.equals(RepositoryTypeEnum.PROXY.getType()))
        { this.downloadProxy(repository, httpHeaders, path, request, response);}
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 400, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = { "{storageId}/{repositoryId}/archive/refs/heads/{path:.+}" })
    public void downloadBranchZip(@RepoMapping Repository repository,
                               @RequestHeader HttpHeaders httpHeaders,
                               @PathVariable String path,
                               HttpServletRequest request,
                               HttpServletResponse response)
            throws Exception {
        final String type = repository.getType();
        if (type.equals(RepositoryTypeEnum.HOSTED.getType())) {

        } else if (type.equals(RepositoryTypeEnum.PROXY.getType())) {
            this.downloadBranch(repository, httpHeaders, path, request, response);
        }
    }


    private void downloadHosted(Repository repository,
                                HttpHeaders httpHeaders,
                                String path,
                                HttpServletRequest request,
                                HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }
    
    private void downloadProxy(Repository repository,
                               HttpHeaders httpHeaders,
                               String path,
                               HttpServletRequest request,
                               HttpServletResponse response) throws Exception
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

//        final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        final Pattern compile = Pattern.compile("pod/git/(.*?)/(.*?)/(.*?)$");
        final Matcher matcher = compile.matcher(path);
        if (!matcher.find()) 
        { 
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
    
            if (Files.exists(repositoryPath))
            { // 如果已经缓存过，则直接返回下载
                vulnerabilityBlock(repositoryPath);
                provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            }
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        
        final String owner = matcher.group(1);
        final String podName = matcher.group(2);
        final String version = matcher.group(3);
        final String artifactCacheFolderPath = String.format("%s/%s/tags/%s/temp", owner, podName, version);

        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);
        final String targetUrl = String.format("https://github.com/%s/%s/archive/refs/tags/%s.zip", owner, podName, version),
                artifactZipCachePath = String.format("%s/%s-%s.zip", artifactCacheFolderPath, podName, version), 
                artifactTarGzPath = String.format("%s/%s/tags/%s/%s-%s.tar.gz", owner, podName, version, podName, version);

//        RepositoryPath repositoryTarGzPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactTarGzPath);
        RepositoryPath repositoryTarGzPath = repositoryPathResolver.resolve(repository, artifactTarGzPath);
        if (Files.exists(repositoryTarGzPath))
        { // 在repo-art插件请求下载前判断是否存在，存在则直接返回
            vulnerabilityBlock(repositoryTarGzPath);
            response.setHeader("Content-Disposition", String.format("attachment;filename=%s-%s.tar.gz", podName, version));
            response.setContentType("application/x-gzip");
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryTarGzPath);
            return;
        }

        RepositoryPath artifactZipCacheRPath = null;
        final String artifact2TarGzLocalTempPath = String.format("%s/%s.zip", tempPath, UUID.randomUUID());
        artifactZipCacheRPath = artifactResolutionService.resolvePath(storageId, repositoryId, targetUrl, artifactZipCachePath);
        try (final BufferedInputStream zipInputStream = new BufferedInputStream(Files.newInputStream(artifactZipCacheRPath))){
            // 转存到本地临时文件
            CompressUtil.zipInputSteam2TarGzFile(zipInputStream, artifact2TarGzLocalTempPath);

            // 装在附加信息
            final ArtifactEntity artifactEntity = new ArtifactEntity(storageId, repositoryId, RepositoryFiles.readCoordinates(repositoryTarGzPath));
            final CocoapodsCoordinates artifactCoordinates = (CocoapodsCoordinates) artifactEntity.getArtifactCoordinates();
            artifactCoordinates.setVersion(version);
            artifactCoordinates.setBaseName(podName);
            repositoryTarGzPath.setArtifact(artifactEntity);
            // 将本地临时TarGz文件转存到Folib
            try (InputStream inputStream = Files.newInputStream(Path.of(artifact2TarGzLocalTempPath))) {
                artifactManagementService.store(repositoryTarGzPath, inputStream);
            }
            artifactManagementService.validateAndStoreIndex(repositoryTarGzPath);
            
            vulnerabilityBlock(repositoryTarGzPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryTarGzPath);
        }
        finally
        {
            // 删除临时目录
            if (null != artifactZipCacheRPath)
            { artifactManagementService.delete(artifactZipCacheRPath.getParent(), true); }
            final File artifact2TarGzLocalTempFile = new File(artifact2TarGzLocalTempPath);
            if (FileUtil.exist(artifact2TarGzLocalTempFile))
            { FileUtil.del(artifact2TarGzLocalTempFile); }
        }
    }

    public void downloadBranch(Repository repository,
                               HttpHeaders httpHeaders,
                               String path,
                               HttpServletRequest request,
                               HttpServletResponse response) throws Exception {


        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/master.tar.gz");

        if (!Files.exists(repositoryPath)) { // 如果未发现索引文件，进行索引同步
            try {
                final boolean syncProxyIndexResult = cocoapodsIndexService.syncProxyIndex(repository);
                logger.info("同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId), syncProxyIndexResult ? "成功" : "失败");
            } catch (Exception e) {
                logger.error("同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId), "异常");
                logger.error(e.getMessage());
                response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
                return;
            }
        } else {
            logger.info("仓库（{}）复用已存在索引文件，跳过同步索引逻辑", String.format("%s:%s", storageId, repositoryId));
        }

        response.setHeader("Content-Disposition", "attachment;filename="+path);
        response.setContentType("application/x-gzip");
        try (final ServletOutputStream outputStream = response.getOutputStream();
             final InputStream fileInputStream = new BufferedInputStream(Files.newInputStream(repositoryPath));
        ) {
            int len = 0;
            byte[] buffer = new byte[8192];
            while ((len = fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }
        }

    }

}
