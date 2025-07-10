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
package com.folib.task;


import cn.hutool.core.date.DateUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.folib.booters.PropertiesBooter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * 日志处理task
 */
@Slf4j
@Component
@EnableScheduling
public class LogsTask {

    @Inject
    @Lazy
    private PropertiesBooter propertiesBooter;

    /**
     * 每天4点清理日志
     */
    @Scheduled(cron = "0 0 4 * * ?")
    public void run() {
        log.info("LogsTask starting time [{}]", DateUtil.now());
        handle();
        log.info("LogsTask end time [{}]", DateUtil.now());
    }

    public void handle() {
        Path vaultPath = Path.of(propertiesBooter.getVaultDirectory());
        if (Files.exists(vaultPath)) {
            Path logsPath = vaultPath.resolve("logs");
            List<String> suffixDeleteFilenameList = Lists.newArrayList(".gz", ".log");
            if (Files.exists(logsPath)) {
                try {
                    Files.walkFileTree(logsPath, new SimpleFileVisitor<Path>() {
                        @Override
                        public FileVisitResult preVisitDirectory(Path dir,
                                                                 BasicFileAttributes attrs)
                                throws IOException {
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult visitFile(Path file,
                                                         BasicFileAttributes attrs)
                                throws IOException {
                            try {
                                String filename = file.getFileName().toString();
                                boolean delete = suffixDeleteFilenameList.stream().anyMatch(filename::endsWith) && compareTime(file);
                                if (delete) {
                                    Files.deleteIfExists(file);
                                    log.info("Delete logsPath [{}] finished", file.toString());
                                }
                            } catch (Exception ex) {
                                log.error("Delete logsPath [{}] error [{}]", file.toString(), ExceptionUtils.getStackTrace(ex));
                            }
                            return FileVisitResult.CONTINUE;
                        }
                    });
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
        }
    }

    private boolean compareTime(Path path) {
        boolean confirmDelete = false;
        int minusDays = 31;
        LocalDateTime fileUpdateTime = getFileUpdateTime(path);
        log.info("Path [{}] fileUpdateTime [{}]", path.toString(), fileUpdateTime);
        if (Objects.isNull(fileUpdateTime)) {
            return true;
        }
        String history = SpringUtil.getProperty("logging.file.history");
        Integer maxHistory = null;
        if (StringUtils.isNotBlank(history)) {
            try {
                maxHistory = Integer.parseInt(history);
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        if (Objects.nonNull(maxHistory)) {
            minusDays = maxHistory;
        }
        //删除过期的文件
        if (!LocalDateTime.now().minusDays(minusDays).isBefore(fileUpdateTime)) {
            confirmDelete = true;
        }
        return confirmDelete;
    }

    private LocalDateTime getFileUpdateTime(Path path) {
        LocalDateTime lastModifiedDateTime = null;
        try {
            BasicFileAttributes attributes = Files.readAttributes(path, BasicFileAttributes.class);
            FileTime fileTime = attributes.lastModifiedTime();
            // 将FileTime转换为Instant
            Instant instant = fileTime.toInstant();
            // 将Instant转换为LocalDateTime
            lastModifiedDateTime = instant.atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime();
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return lastModifiedDateTime;
    }
}
