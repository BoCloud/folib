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
package com.folib.components.files;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.folib.enums.FileUnitTypeEnum;
import com.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2024/11/29
 **/
@Slf4j
@Component
public class FilesCommonComponent {

    public void storeContent(String content, String logPath) {
        try {
            if (StringUtils.isBlank(content)) {
                return;
            }
            storeLog(content, logPath);
        } catch (Exception ex) {
            log.error("Store content error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    private void storeLog(String content, String logPath) throws IOException {
        Path writePath = getLogPath(getLatestIndex(logPath), logPath);
        //追加写模式
        try (BufferedWriter writer = Files.newBufferedWriter(writePath, StandardCharsets.UTF_8, StandardOpenOption.APPEND)) {
            writer.write(content + System.lineSeparator());
        }
    }

    private Path getLogPath(Integer index, String logPath) throws IOException {
        if (Objects.isNull(index)) {
            index = 1;
        }
        String filename = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATE_PATTERN) + "_index_%s.txt";
        String filePath = logPath + File.separator + String.format(filename, index);
        log.debug("Log file path [{}]", filePath);
        Path path = Path.of(filePath);
        Files.createDirectories(path.getParent());
        //每个文件10M大小
        BigDecimal maxSize = BigDecimal.valueOf(10);
        if (!Files.exists(path)) {
            Files.createFile(path);
        }
        if (FileSizeConvertUtils.convertBytesWithDecimal(Files.size(path), FileUnitTypeEnum.MB.getUnit()).compareTo(maxSize) >= 0) {
            return getLogPath(index + 1, logPath);
        }
        return path;
    }

    private Integer getLatestIndex(String filePath) {
        Path path = Path.of(filePath);
        if (Files.exists(path)) {
            try (Stream<Path> pathStream = Files.list(path)) {
                List<Path> pathList = pathStream.sorted().collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(pathList)) {
                    Path logPath = pathList.get(pathList.size() - 1);
                    String filename = logPath.getFileName().toString();
                    filename = FilenameUtils.getBaseName(filename);
                    List<String> nameSplitList = Arrays.asList(filename.split("_"));
                    if (CollectionUtils.isNotEmpty(nameSplitList)) {
                        String index = nameSplitList.get(nameSplitList.size() - 1);
                        return Integer.parseInt(index);
                    }
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return null;
    }
}
