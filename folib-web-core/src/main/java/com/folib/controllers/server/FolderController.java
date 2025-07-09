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
package com.folib.controllers.server;

import com.google.common.collect.Lists;
import com.folib.domain.FolderInfo;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.File;
import java.util.*;

/**
 * @author veadan
 * @date 2023/9/26
 **/
@PreAuthorize("hasAuthority('ADMIN')")
@RestController
@RequestMapping("/api/folder")
@Api(description = "目录", tags = "目录")
public class FolderController {

    /**
     * 获取目录列表
     *
     * @param directoryPath 目录名
     * @return 目录列表
     */
    @ApiOperation(value = "获取目录列表", response = FolderInfo.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping("/list")
    public ResponseEntity<List<FolderInfo>> getFoldersInDirectory(@RequestParam(required = false) String directoryPath, @RequestParam(required = false) String includesSuffix) {
        if (directoryPath == null) {
            directoryPath = "/";
        }
        File directory = new File(directoryPath);
        List<FolderInfo> folderList = Lists.newArrayList();
        if (!directory.exists() || !directory.isDirectory()) {
            return ResponseEntity.ok(folderList);
        }
        // 如果 includesSuffix 不为null，拆分多个后缀
        final Set<String> suffixSet = StringUtils.isNoneBlank(includesSuffix)
                ? new HashSet<>(Arrays.asList(includesSuffix.split(",")))
                : Collections.emptySet();
        // 使用FilenameFilter进行后缀过滤
        File[] subDirectories = directory.listFiles((dir, name) -> {
            File file = new File(dir, name);
            // 如果提供了 includesSuffix，且文件是目录并且文件名符合后缀
            return file.isDirectory() || (CollectionUtils.isNotEmpty(suffixSet) && suffixSet.stream().anyMatch(name::endsWith));
        });
        if (subDirectories != null) {
            for (File subDirectory : subDirectories) {
                folderList.add(new FolderInfo(subDirectory.getName(), subDirectory.getAbsolutePath(), hasSubDirectories(subDirectory), subDirectory.isFile()));
            }
        }
        return ResponseEntity.ok(folderList);
    }

    /**
     * 判断是否有下级子目录
     *
     * @param directory 目录名
     * @return true 有子目录 false 无子目录
     */
    private boolean hasSubDirectories(File directory) {
        File[] subDirectories = directory.listFiles(File::isDirectory);
        return subDirectories != null && subDirectories.length > 0;
    }

}
