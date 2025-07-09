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
import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryPath;
import com.folib.service.CocoapodsIndexService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.CocoapodsArtifactUtil;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import jakarta.servlet.ServletOutputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/***
 *
 * @author veadan
 * @date 2023/8/2 14:58
 */
@RestController
@LayoutReqMapping(CocoapodsCoordinates.LAYOUT_NAME)
public class CocoapodsIndexController
        extends BaseArtifactController {

    @Inject
    private CocoapodsIndexService cocoapodsIndexService;
    
    
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}/index/fetchIndex")
    public ResponseEntity repoArtIndex(@RepoMapping Repository repository, HttpServletRequest request, HttpServletResponse response) throws Exception {
        final String type = repository.getType();

        ResponseEntity responseEntity = null;
        if (type.equals(RepositoryTypeEnum.HOSTED.getType()))
        { responseEntity = this.repoArtIndexHosted(repository, response); }
        else if (type.equals(RepositoryTypeEnum.PROXY.getType()))
        { responseEntity = this.repoArtIndexProxy(repository, response);}
        else
        { return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("invalid repository type"); }
        if (null != responseEntity)
        { return responseEntity; }

        return new ResponseEntity<>("ok", HttpStatus.OK);
    }
    
    private ResponseEntity repoArtIndexHosted(Repository repository, HttpServletResponse response)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RepositoryPath repositoryPath;
        try 
        { repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, ".specs/"); } 
        catch (IOException e) 
        { return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage()); }
        final Path repositoryPathTarget = repositoryPath.getTarget();
        final String baseUrl = super.getBaseUrl();

        ServletOutputStream servletOutputStream = null;
        TarArchiveOutputStream tarArchiveOutputStream = null;
        try
        {
            response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
            response.setContentType("application/x-gzip");
            
            final String indexFolder = repositoryPathTarget.toUri().getPath();
            servletOutputStream = response.getOutputStream();
            tarArchiveOutputStream = new TarArchiveOutputStream(new GzipCompressorOutputStream(servletOutputStream));
            tarArchiveOutputStream.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            this.tarGzLocalFolder(repository, baseUrl, indexFolder, indexFolder, tarArchiveOutputStream);
            servletOutputStream.flush();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally {
            try {
                if (null != tarArchiveOutputStream)
                { tarArchiveOutputStream.close(); }
                if (null != servletOutputStream)
                { servletOutputStream.close(); }
            } catch (Exception e) {
                logger.error("关闭zip文件流失败", e);
            }
        }
        
        return null;
    }

    private ResponseEntity repoArtIndexProxy(Repository repository, HttpServletResponse response) throws Exception 
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/master.tar.gz");
        
        if (!Files.exists(repositoryPath))
        { // 如果未发现索引文件，进行索引同步
            try 
            {
                final boolean syncProxyIndexResult = cocoapodsIndexService.syncProxyIndex(repository);
                logger.info("同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId),syncProxyIndexResult?"成功":"失败");
            }
            catch (Exception e)
            {
                return ResponseEntity
                        .status(HttpStatus.INTERNAL_SERVER_ERROR)
                        .body(e.getMessage());
            }
        }
        else
        { logger.info("仓库（{}）复用已存在索引文件，跳过同步索引逻辑", String.format("%s:%s", storageId, repositoryId)); }

        response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
        response.setContentType("application/x-gzip");
        try (final ServletOutputStream outputStream = response.getOutputStream();
             final InputStream fileInputStream = new BufferedInputStream(Files.newInputStream(repositoryPath));
        ){
            int len = 0;
            byte[] buffer = new byte[1024];
            while((len=fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, len);
            }
        }
        
        return ResponseEntity.ok().build();
    }
        
    
    private void tarGzLocalFolder(Repository repository, String baseUrl, String rootPath, String srcFolder, TarArchiveOutputStream archiveOutputStream) throws Exception 
    {
        final File folder = new File(srcFolder);
        //遍历文件夹下所有的文件和文件夹
        final File[] files = folder.listFiles();
        
        if (null != files)
        {
            for (File file : files) 
            {
                //如果是文件夹,递归压缩
                if (file.isDirectory()) {
                    tarGzLocalFolder(repository, baseUrl, rootPath, file.getAbsolutePath(), archiveOutputStream);
                    continue;
                }
    
                if (file.getName().endsWith(".podspec"))
                {
                    byte[] bytes = FileUtil.readBytes(file);
                    final String podspecFileUri = file.getAbsolutePath().replace(rootPath, StringUtils.EMPTY);
                    
                    final RepositoryPath repositoryPath = artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), String.format(".specs/%s", podspecFileUri));
                    if (repositoryPath != null  && null != repositoryPath.getArtifactEntry())
                    { // 获取到有Pod源代码路径地址则替换
                        final Artifact artifactEntry = repositoryPath.getArtifactEntry();
                        final String path = artifactEntry.getArtifactCoordinates().getPath();
                        final String newSourceUrl = String.format("%s/%s%s", baseUrl, "storages", path);
                        final String newPodspecContent = CocoapodsArtifactUtil.replaceNewSourceUrlOfPodspecContent(new String(bytes), newSourceUrl);
                        if (StringUtils.isNotBlank(newPodspecContent))
                        { bytes = newPodspecContent.getBytes(StandardCharsets.UTF_8); }
                    }

                    final TarArchiveEntry entry = new TarArchiveEntry(podspecFileUri);
                    entry.setSize(bytes.length);
                    archiveOutputStream.putArchiveEntry(entry);
                    archiveOutputStream.write(bytes);
                    archiveOutputStream.closeArchiveEntry();
                }
            }
        }
    }
}
