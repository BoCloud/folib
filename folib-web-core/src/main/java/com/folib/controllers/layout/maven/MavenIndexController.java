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

import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.RepositoryPath;
import com.folib.controllers.BaseController;
import com.folib.providers.layout.IndexingDisabledException;
import com.folib.providers.layout.MavenFileSystem;
import com.folib.storage.repository.Repository;
import com.folib.web.RepoMapping;

import javax.inject.Inject;
import java.io.IOException;

import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import static com.folib.config.Maven2LayoutProviderConfig.FILE_SYSTEM_ALIAS;

/**
 * @author Kate Novik
 * @author Veadan
 * @author veadan
 */
@RestController
@RequestMapping("/api/maven/index")
@Api(description = "maven索引控制器",tags = "maven索引控制器")
public class MavenIndexController
        extends BaseController
{

    @Inject
    @Qualifier(FILE_SYSTEM_ALIAS)
    private LayoutFileSystemFactory layoutFileSystemFactory;

    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @PostMapping(value = "/{storageId}/{repositoryId}", produces = { MediaType.TEXT_PLAIN_VALUE,
                                                                     MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity rebuildIndex(@RepoMapping Repository repository)
    {
        try
        {
            LayoutFileSystem layoutFileSystem = layoutFileSystemFactory.create(repository);
            RepositoryPath indexPath = ((MavenFileSystem) layoutFileSystem).rebuildIndex(repository);

            return ResponseEntity.ok(String.format("Index was regenerated in [%s].", indexPath));
        }
        catch (IndexingDisabledException e)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Indexing is disabled on this repository.");
        }
        catch (IOException e)
        {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                                 .body(e.getMessage());
        }
    }
}
