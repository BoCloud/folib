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

import com.folib.providers.io.RepositoryPath;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.repository.Repository;
import com.folib.web.RepoMapping;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.nio.file.Files;

import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * @author veadan
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@Api(description = "JFrog删除", tags = "JFrog删除")
public class ArtifactDeleteController extends JFrogBaseController {

    @ApiOperation(value = "Deletes a path from a artifact.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deleted."),
            @ApiResponse(code = 500, message = "Internal server error."),
            @ApiResponse(code = 404, message = "The specified repositoryId/path does not exist!")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DELETE')")
    @DeleteMapping(value = "/{repositoryId:^(?!api$).+}/{artifactPath:.+}")
    public ResponseEntity delete(@RepoMapping Repository repository,
                                 @ApiParam(value = "Whether to use force delete")
                                 @RequestParam(defaultValue = "false",
                                         name = "force",
                                         required = false) boolean force,
                                 @PathVariable String artifactPath)
            throws IOException {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body(String.format("The specified storageId [%s] repositoryId [%s] path [%s] does not exist!", storageId, repositoryId, artifactPath));
            }
            artifactManagementService.delete(repositoryPath, force);
        } catch (ArtifactStorageException e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("The artifact was deleted.");
    }

}
