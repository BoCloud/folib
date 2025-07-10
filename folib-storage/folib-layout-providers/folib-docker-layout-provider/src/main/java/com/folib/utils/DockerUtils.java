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
package com.folib.utils;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.DockerSubsidiary;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2024/7/11
 **/
@Slf4j
public class DockerUtils {

    public static final String SUBSIDIARY_PATH = "subsidiaryArtifact";

    public static final String SUBSIDIARY = File.separator + SUBSIDIARY_PATH + File.separator;

    public static RepositoryPath getDockerSubsidiaryPath(RepositoryPath repositoryPath) {
        if (Files.isDirectory(repositoryPath)) {
            return repositoryPath.resolve(SUBSIDIARY_PATH);
        }
        return repositoryPath.resolveSibling(SUBSIDIARY_PATH);
    }

    public static List<DockerSubsidiary> getDockerSubsidiaryFilePaths(RepositoryPath repositoryPath) {
        List<DockerSubsidiary> dockerSubsidiaries = Lists.newArrayList();
        try {
            if (!DockerCoordinates.isDockerTag(repositoryPath)) {
                return null;
            }
            if (Files.isSameFile(repositoryPath.getRoot(), repositoryPath)) {
                return null;
            }
            RepositoryPath dockerSubsidiaryRepositoryPath = getDockerSubsidiaryPath(repositoryPath);
            if (!Files.exists(dockerSubsidiaryRepositoryPath)) {
                return null;
            }
            String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
            try (Stream<Path> pathStream = Files.walk(dockerSubsidiaryRepositoryPath)) {
                pathStream
                        .forEach(p -> {
                            try {
                                RepositoryPath subRepositoryPath = (RepositoryPath) p;
                                if (!RepositoryFiles.isHidden(subRepositoryPath) && !RepositoryFiles.isChecksum(subRepositoryPath) && !RepositoryFiles.isArtifactMetadata(subRepositoryPath) && !Files.isDirectory(subRepositoryPath)) {
                                    String path = RepositoryFiles.relativizePath(subRepositoryPath);
                                    dockerSubsidiaries.add(DockerSubsidiary.builder().path(path).name(subRepositoryPath.getFileName().toString()).url(repositoryBaseUrl + path).build());
                                }
                            } catch (Exception ex) {
                                log.error("DockerSubsidiaryFilePaths error [{}]", ExceptionUtils.getStackTrace(ex));
                            }
                        });
                return dockerSubsidiaries;
            } catch (Exception ex) {
                log.error("DockerSubsidiaryFilePaths error [{}]", ExceptionUtils.getStackTrace(ex));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return dockerSubsidiaries;
    }

    protected static String getRepositoryBaseUrl(Repository repository) {
        ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
        return String.format("%s/storages/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    public static boolean isSubsidiaryFile(RepositoryPath path) {
        try {
            if (path.getRoot().toAbsolutePath().toString().equals(path.toAbsolutePath().toString())) {
                return false;
            }
            String parentFilename = path.getParent().getFileName().toString();
            return Objects.equals(DockerUtils.SUBSIDIARY_PATH, parentFilename);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }
}
