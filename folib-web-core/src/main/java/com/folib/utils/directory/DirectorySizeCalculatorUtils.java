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
package com.folib.utils.directory;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * 目录大小计算工具类
 *
 * @author veadan
 * @date 2024/7/18
 **/
@Slf4j
public class DirectorySizeCalculatorUtils {

    /**
     * 目录
     */
    private final RepositoryPath directory;

    public DirectorySizeCalculatorUtils(RepositoryPath directory) {
        this.directory = directory;
    }

    public Result compute() {
        final Result result = new Result();
        try {
            if (!Files.exists(directory) || !Files.exists(directory.getTarget())) {
                log.warn("DirectorySizeCalculatorUtils directory [{}] not exists", directory);
                return result;
            }
            RepositoryPathResolver repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
            String storageId = directory.getStorageId(), repositoryId = directory.getRepositoryId();
            String repositoryPrefix = String.format("/%s/%s/", storageId, repositoryId);
            Files.walkFileTree(directory.getTarget(), new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    log.warn(exc.getMessage());
                    // 目录或文件已删除，继续遍历
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    try {
                        String path = file.toString(), name = file.getFileName().toString();
                        if (exclude(name)) {
                            log.warn("Visit file path [{}] skip...", file);
                            return FileVisitResult.CONTINUE;
                        }
                        String artifactPath = path.substring(path.indexOf(repositoryPrefix) + repositoryPrefix.length());
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        result.incrementFilesCount();
                        result.addToFilesSize(attrs.size());
                        log.debug("Pre visit directory [{}] path [{}]", directory, path);
                        if (RepositoryFiles.isArtifact(repositoryPath)) {
                            result.incrementArtifactsCount();
                            result.addToArtifactsSize(attrs.size());
                        }
                    } catch (Exception ex) {
                        log.error("DirectorySizeCalculatorUtils path [{}] error [{}]", file, ExceptionUtils.getStackTrace(ex));
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    try {
                        if (Files.isSameFile(directory, dir)) {
                            result.incrementDirectoriesCount();
                            return FileVisitResult.CONTINUE;
                        }
                        log.debug("Pre visit directory [{}] dir [{}]", directory, dir);
                        String path = dir.toString();
                        String artifactPath = path.substring(path.indexOf(repositoryPrefix) + repositoryPrefix.length());
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        result.incrementDirectoriesCount();
                    } catch (Exception ex) {
                        log.error("DirectorySizeCalculatorUtils dir [{}] error [{}]", dir, ExceptionUtils.getStackTrace(ex));
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            log.error("DirectorySizeCalculatorUtils directory [{}] error [{}]", directory, ExceptionUtils.getStackTrace(e));
        }
        return result;
    }

    public static boolean exclude(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        String dsStore = ".DS_Store";
        if (name.endsWith(dsStore)) {
            return true;
        }
        return false;
    }

}

