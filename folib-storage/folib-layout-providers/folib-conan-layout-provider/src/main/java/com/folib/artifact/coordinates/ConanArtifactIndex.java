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
package com.folib.artifact.coordinates;

import com.folib.constant.GlobalConstants;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author veadan
 * @date 2024/3/20
 **/
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@Slf4j
public class ConanArtifactIndex {

    public static final String INDEX_JSON_NAME = "index.json";

    private String user;
    private String name;
    private String version;
    private String channel;
    private String revisionId;
    private String packageId;
    private String indexRelativizePath;
    private String rootIndexRelativizePath;

    public static ConanArtifactIndex parse(String relativizePath) {
        if (StringUtils.isBlank(relativizePath)) {
            return null;
        }
        ConanArtifactIndex conanArtifactIndex = null;
        String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
        if (pathArr.length >= 7) {
            String packagePath = "";
            if (relativizePath.contains("/package/")) {
                packagePath = packagePath.concat(GlobalConstants.SEPARATOR).concat("package");
            }
            String indexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]).concat(GlobalConstants.SEPARATOR).concat(pathArr[4]).concat(packagePath).concat(GlobalConstants.SEPARATOR).concat(pathArr[6]);
            String rootIndexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]);
            conanArtifactIndex = ConanArtifactIndex.builder().user(pathArr[0]).name(pathArr[1]).version(pathArr[2]).channel(pathArr[3]).revisionId(pathArr[4]).packageId(pathArr[6]).indexRelativizePath(indexRelativizePath).rootIndexRelativizePath(rootIndexRelativizePath).build();
        } else if (pathArr.length >= 4) {
            String indexRelativizePath = pathArr[0].concat(GlobalConstants.SEPARATOR).concat(pathArr[1]).concat(GlobalConstants.SEPARATOR).concat(pathArr[2]).concat(GlobalConstants.SEPARATOR).concat(pathArr[3]);
            conanArtifactIndex = ConanArtifactIndex.builder().user(pathArr[0]).name(pathArr[1]).version(pathArr[2]).channel(pathArr[3]).indexRelativizePath(indexRelativizePath).build();
        }
        return conanArtifactIndex;
    }

    public static boolean isIndexDirectory(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length == 7 || pathArr.length == 4) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    public static boolean isReferenceIndexJSON(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length == 5 && repositoryPath.getFileName().toString().endsWith(ConanArtifactIndex.INDEX_JSON_NAME)) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    public static boolean include(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = !Files.isDirectory(path) || RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            return Files.isDirectory(path);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }
}
