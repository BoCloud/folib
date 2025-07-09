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
package com.folib.controllers.layout.maven;

import com.folib.storage.ArtifactStorageException;
import com.folib.controllers.BaseController;
import com.folib.services.ArtifactMetadataService;
import com.folib.storage.metadata.MetadataType;
import com.folib.storage.repository.Repository;
import com.folib.web.RepoMapping;

import javax.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import io.swagger.annotations.*;
import org.apache.maven.artifact.ArtifactUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

/**
 * @author Martin Todorov
 */
@Controller
@RequestMapping("/api/maven/metadata")
@Api(description = "maven metadata管理控制器",tags = "maven metadata管理控制器")
@PreAuthorize("hasAuthority('ROOT')")
public class MavenMetadataManagementController
        extends BaseController
{


    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @ApiOperation(value = "Used to rebuild the metadata for a given path.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The metadata was successfully rebuilt!"),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_METADATA')")
    @PostMapping(produces = MediaType.TEXT_PLAIN_VALUE)
    public ResponseEntity rebuild(@ApiParam(value = "The storageId", required = true)
                                  @RequestParam("storageId") String storageId,
                                  @ApiParam(value = "The repositoryId")
                                  @RequestParam(value = "repositoryId", required = false) String repositoryId,
                                  @ApiParam(value = "The path")
                                  @RequestParam(value = "path", required = false) String path)
            throws IOException,
                   NoSuchAlgorithmException,
                   XmlPullParserException
    {
        try
        {
            if (storageId != null && repositoryId != null)
            {
                artifactMetadataService.rebuildMetadata(storageId, repositoryId, path);
            }
            else if (storageId != null && repositoryId == null)
            {
                artifactMetadataService.rebuildMetadata(storageId, path);
            }

            return ResponseEntity.ok("The metadata was successfully rebuilt!");
        }
        catch (ArtifactStorageException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(e.getMessage());
        }
    }

    @ApiOperation(value = "Used to delete metadata entries for an artifact")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successfully removed metadata entry."),
                            @ApiResponse(code = 500, message = "An error occurred.") })
    @PreAuthorize("hasAuthority('MANAGEMENT_DELETE_METADATA')")
    @DeleteMapping(value = "{storageId}/{repositoryId}/{path:.+}",
                   produces = MediaType.TEXT_PLAIN_VALUE)
    public ResponseEntity delete(@RepoMapping Repository repository,
                                 @ApiParam(value = "The version of the artifact.", required = true)
                                 @RequestParam(name = "version") String version,
                                 @ApiParam(value = "The classifier of the artifact.")
                                 @RequestParam(name = "classifier") String classifier,
                                 @ApiParam(value = "The type of metadata (artifact/snapshot/plugin).")
                                 @RequestParam(name = "metadataType") String metadataType,
                                 @PathVariable String path)
            throws IOException,
                   NoSuchAlgorithmException,
                   XmlPullParserException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Deleting metadata for {}:{}:{}:{}...",  storageId, repositoryId, path, version);

        try
        {
            if (!ArtifactUtils.isSnapshot(version))
            {
                artifactMetadataService.removeVersion(storageId,
                                                      repositoryId,
                                                      path,
                                                      version,
                                                      MetadataType.from(metadataType));
            }
            else
            {
                artifactMetadataService.removeTimestampedSnapshotVersion(storageId,
                                                                         repositoryId,
                                                                         path,
                                                                         version,
                                                                         classifier);
            }

            return ResponseEntity.ok("Successfully removed metadata entry.");
        }
        catch (ArtifactStorageException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(e.getMessage());
        }
    }

}
