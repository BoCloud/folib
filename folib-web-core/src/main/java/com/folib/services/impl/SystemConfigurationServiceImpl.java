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
package com.folib.services.impl;

import cn.hutool.core.util.ZipUtil;
import com.google.common.collect.Lists;
import com.folib.booters.PropertiesBooter;
import com.folib.model.request.ExportSystemConfigurationReq;
import com.folib.model.request.ImportSystemConfigurationReq;
import com.folib.services.SystemConfigurationService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2025/3/28
 **/
@Slf4j
@Service
public class SystemConfigurationServiceImpl implements SystemConfigurationService {

    @Autowired
    private PropertiesBooter propertiesBooter;

    @Override
    public void exportSystemConfiguration(ExportSystemConfigurationReq exportSystemConfiguration) {
        try {
            Path targetParentPath = Path.of(exportSystemConfiguration.getPath());
            if (!Files.exists(targetParentPath)) {
                Files.createDirectories(targetParentPath);
            }
            // 统一权限检查逻辑，避免重复代码
            checkPermission(Files.isExecutable(targetParentPath), "执行", targetParentPath);
            checkPermission(Files.isReadable(targetParentPath), "读", targetParentPath);
            checkPermission(Files.isWritable(targetParentPath), "写", targetParentPath);
            Path confPath = getConfPath();
            if (Boolean.TRUE.equals(exportSystemConfiguration.getZipArchive())) {
                Path targetZipPath = targetParentPath.resolve("conf.zip");
                ZipUtil.zip(confPath.toAbsolutePath().toString(), targetZipPath.toAbsolutePath().toString(), false);
                return;
            }
            try (Stream<Path> pathStream = Files.list(confPath)) {
                pathStream.forEach(path -> {
                    try {
                        Path targetPath = targetParentPath.resolve(path.getFileName().toString());
                        if (Files.isRegularFile(path) && getIncludeFilenames().stream().anyMatch(item -> item.equals(path.getFileName().toString()))) {
                            FileUtils.copyFile(path.toFile(), targetPath.toFile());
                        }
                    } catch (IOException ex) {
                        throw new RuntimeException(ex);
                    }
                });
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void importSystemConfiguration(ImportSystemConfigurationReq importSystemConfiguration) {
        try {
            Path importPath = Path.of(importSystemConfiguration.getPath());
            if (Files.notExists(importPath)) {
                throw new RuntimeException("Not found import path or zip file");
            }
            Path confPath = getConfPath();
            if (Files.isDirectory(importPath)) {
                try (Stream<Path> pathStream = Files.list(importPath)) {
                    pathStream.forEach(path -> {
                        try {
                            Path targetPath = confPath.resolve(path.getFileName().toString());
                            if (Files.isRegularFile(path) && getIncludeFilenames().stream().anyMatch(item -> item.equals(path.getFileName().toString()))) {
                                FileUtils.copyFile(path.toFile(), targetPath.toFile());
                            }
                        } catch (IOException ex) {
                            throw new RuntimeException(ex);
                        }
                    });
                }
            } else {
                ZipUtil.unzip(importPath.toAbsolutePath().toString(), confPath.toAbsolutePath().toString());
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    private Path getConfPath() {
        Path etcPath = Paths.get(propertiesBooter.getEtcDirectory());
        String conf = "conf";
        return etcPath.resolve(conf);
    }

    private List<String> getIncludeFilenames() {
        List<String> includeFilenames = Lists.newArrayList();
        includeFilenames.add("cassandra.yaml");
        includeFilenames.add("db_EMBEDDED.yaml");
        includeFilenames.add("db_MEMORY.yaml");
        includeFilenames.add("db_REMOTE.yaml");
        includeFilenames.add("folib.yaml");
        includeFilenames.add("folib-authentication-providers.yaml");
        includeFilenames.add("folib-authorization.yaml");
        includeFilenames.add("folib-cron-tasks.yaml");
        includeFilenames.add("folib-security-users.yaml");
        includeFilenames.add("janusgraph-cassandra.properties");
        includeFilenames.add("janusgraph-inmemory.properties");
        return includeFilenames;
    }

    // 辅助方法用于权限检查
    private  void checkPermission(boolean hasPermission, String permissionName, Path path) throws IOException {
        if (!hasPermission) {
            throw new IOException(String.format("目标路径[%s]没有%s权限", path, permissionName));
        }
    }

}
