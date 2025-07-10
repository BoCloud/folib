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
package com.folib.controllers.layout.debian;

import com.folib.domain.debian.DebianParserVO;
import com.folib.domain.debian.DebianUploadBO;
import com.folib.services.DebianService;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-09-06 10:36
 */


@RestController
@RequestMapping("api/debian")
public class DebianStorageController{

    @Resource
    private DebianService debianService;

    @PostMapping("parseArtifact")
    public ResponseEntity<DebianParserVO> parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        return ResponseEntity.ok(debianService.parseArtifact(storageId, repositoryId, file));
    }

    @PostMapping("upload")
    public ResponseEntity<String> upload(@Validated @RequestBody DebianUploadBO uploadBO) {
        return ResponseEntity.ok(debianService.upload(uploadBO));
    }
    @PostMapping("batchUpload")
    public ResponseEntity<String> batchUpload(List<MultipartFile> files, String storageId, String repositoryId, String distribution,String component) {
        debianService.batchUpload(files,storageId,repositoryId,distribution,component);
        return ResponseEntity.ok("accepted");
    }






}
