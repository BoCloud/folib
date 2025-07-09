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
package com.folib.controllers;

import com.folib.forms.scanner.*;
import com.folib.services.ArtifactWebService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/scanner")
@Api(description="组件扫描",tags = "组件扫描")
public class ScannerController extends BaseController {

    @Autowired
    private ArtifactWebService artifactWebService;

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/getCount")
    public ResponseEntity<CountForm> getCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.getCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/weekCount")
    public ResponseEntity<WeekCountForm> weekCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.weekCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/monthCount")
    public ResponseEntity<List<DayCountForm>> monthCount(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.monthCount(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/repositories")
    public ResponseEntity<List<RepositoryCountForm>> repositories(Authentication authentication) {
        return ResponseEntity.ok(artifactWebService.repositories(authentication));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping("/repository")
    public ResponseEntity<RepositoryScannerForm> repository(@RequestParam("storage") String storage, @RequestParam("repository") String repository, String artifactName, Integer page, Integer limit) {
        return ResponseEntity.ok(artifactWebService.repository(storage, repository, artifactName, page, limit));
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/foEyesEnable")
    public ResponseEntity<Boolean> foEyesEnable() {
        return ResponseEntity.ok(artifactWebService.foEyesEnable());
    }
}
