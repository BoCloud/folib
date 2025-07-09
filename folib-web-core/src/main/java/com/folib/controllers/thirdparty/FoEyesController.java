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
package com.folib.controllers.thirdparty;

import com.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.folib.components.thirdparty.foeyes.reponse.ProjectInfo;
import com.folib.domain.FoEyesConfig;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/4/24
 **/
@RestController
@RequestMapping("/api/foEyes/")
public class FoEyesController {

    @Autowired
    private FoEyesComponent foEyesComponent;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<ProjectInfo> queryProject(@PathVariable String artifactPath,
                                                    @PathVariable String storageId,
                                                    @PathVariable String repositoryId) throws Exception {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return ResponseEntity.ok(null);
        }
        String name = String.format("%s/%s/%s", storageId, repositoryId, RepositoryFiles.relativizePath(repositoryPath));
        return ResponseEntity.ok(foEyesComponent.queryProject(name));
    }

    @GetMapping(value = "/getConfig")
    public ResponseEntity<FoEyesConfig> getConfig() {
        return ResponseEntity.ok(foEyesComponent.getConfig());
    }
}
