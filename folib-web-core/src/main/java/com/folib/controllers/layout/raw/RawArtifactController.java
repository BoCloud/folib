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
package com.folib.controllers.layout.raw;

import com.folib.providers.io.RepositoryPath;
import com.folib.controllers.BaseArtifactController;
import com.folib.providers.RawLayoutProvider;
import com.folib.storage.repository.Repository;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Comparator;
import java.util.Objects;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author Veadan
 */
@RestController
@LayoutReqMapping(RawLayoutProvider.ALIAS)
@Api(description = "raw坐标控制器",tags = "raw坐标控制器")
public class RawArtifactController
        extends BaseArtifactController
{

    @Value("${folib.temp}")
    private String tempPath;
    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The artifact was deployed successfully."),
                            @ApiResponse(code = 400, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{path:.+}")
    public ResponseEntity upload(@RepoMapping Repository repository,
                                 @PathVariable String path,
                                 HttpServletRequest request)
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try (InputStream inputStream = request.getInputStream()){
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);

            return ResponseEntity.ok("The artifact was deployed successfully.");
        }
        catch (Exception e)
        {
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
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, path);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath) && Files.isDirectory(repositoryPath)) {
            String uuid = UUID.randomUUID().toString();
            Path tempPaths = Paths.get(String.join("/", tempPath, uuid));
            Files.createDirectories(tempPaths);
            String baseDirPath = getLastDirectoryDirect(repositoryPath);
            Path zipFilePath = Paths.get(String.join("/", tempPaths.toString(), baseDirPath + ".zip"));
            zipDirectory(repositoryPath.toString(), zipFilePath.toString());
            response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + zipFilePath.getFileName().toString() + "\"");
            response.setContentType("application/octet-stream");
            copyToResponse(Files.newInputStream(Path.of(zipFilePath.toString())), response);
            Files.walk(tempPaths)
                    .sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete);
        } else {
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        }
    }



    public  void zipDirectory(String sourceDirPath, String zipFilePath) throws IOException {
        Path sourceDir = Paths.get(sourceDirPath);
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(zipFilePath))) {
            Files.walkFileTree(sourceDir, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {

                    if(!file.getFileName().toString().endsWith(".md5") &&
                            !file.getFileName().toString().endsWith(".sha1") &&
                            !file.getFileName().toString().endsWith(".sha256") &&
                            !file.getFileName().toString().endsWith(".sm3") &&
                            !file.getFileName().toString().endsWith(".metadata")){
                        zos.putNextEntry(new ZipEntry(sourceDir.relativize(file).toString()));
                        Files.copy(file, zos);
                        zos.closeEntry();
                    }

                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                    zos.putNextEntry(new ZipEntry(sourceDir.relativize(dir).toString() + "/"));
                    zos.closeEntry();
                    return FileVisitResult.CONTINUE;
                }
            });
        }
    }

    private  String getLastDirectoryDirect(Path path) {
        int nameCount = path.getNameCount();
        if (nameCount > 0) {
            return path.subpath(nameCount - 1, nameCount).toString();
        } else {
            // 如果路径为空或只包含一个名称（可能是文件），则返回null或适当的值
            return null; // 或者你可以返回一个表示根目录的Path，例如 Paths.get("/")
        }
    }

}
