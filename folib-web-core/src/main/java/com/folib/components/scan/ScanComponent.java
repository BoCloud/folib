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
package com.folib.components.scan;


import com.folib.constant.GlobalConstants;

import com.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class ScanComponent {

    /**
     * 写入扫描报告
     *
     * @param repositoryPath 制品
     * @param reportContent 扫描结果
     */
    public void writeReport(RepositoryPath repositoryPath, String reportContent) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return;
        }
        if (StringUtils.isBlank(reportContent)) {
            return;
        }
        RepositoryPath reportRepositoryPath = getReportRepositoryPath(repositoryPath);
        try (BufferedWriter writer = Files.newBufferedWriter(reportRepositoryPath, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)) {
            writer.write(reportContent);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * 读取扫描报告
     *
     * @param repositoryPath 制品
     * @return 扫描报告
     */
    public String readReport(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return "";
        }
        RepositoryPath reportRepositoryPath = getReportRepositoryPath(repositoryPath);
        if (!Files.exists(reportRepositoryPath)) {
            return "";
        }
        StringBuilder contentBuilder = new StringBuilder();
        try (BufferedReader reader = Files.newBufferedReader(reportRepositoryPath, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                contentBuilder.append(line).append(System.lineSeparator());
            }
        } catch (Exception ex) {
            log.error("RepositoryPath [{}] read report error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            return "";
        }
        return contentBuilder.toString();
    }

    private RepositoryPath getReportRepositoryPath(RepositoryPath repositoryPath) {
        String artifactMetadataDirectoryName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + GlobalConstants.FO_LIBRARY_METADATA;
        return repositoryPath.resolveSibling(artifactMetadataDirectoryName).resolve("report.json");
    }
}
