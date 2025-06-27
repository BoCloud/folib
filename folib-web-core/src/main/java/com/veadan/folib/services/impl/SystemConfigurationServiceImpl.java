package com.veadan.folib.services.impl;

import cn.hutool.core.util.ZipUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.model.request.ExportSystemConfigurationReq;
import com.veadan.folib.model.request.ImportSystemConfigurationReq;
import com.veadan.folib.services.SystemConfigurationService;
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
