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
package com.folib.job.tasks;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.folib.artifact.coordinates.RawCoordinates;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author veadan
 * @since 2025-02-10 15:21
 */

@Slf4j
@Component
public class ReplicationBackup {

    @Resource
    private ArtifactResolutionService artifactResolutionService;

    @Resource
    private RepositoryPathResolver repositoryPathResolver;

    @Resource

    private ArtifactManagementService artifactManagementService;

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private RepositoryManagementService repositoryManagementService;

    @Value("${folib.temp}")
    private String tempPath;

    public void backUpByPath(Repository repository, Collection<String> paths, String path) {
        // 将增量文件复制到指定目录下
        String prefixPath=tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId()+"/"+path;
        if (Files.exists(Path.of(prefixPath))) {
            try {
                Files.delete(Path.of(prefixPath));
            } catch (IOException e) {
                log.error("delete replication backup failed", e);
            }
        }
        for (String p : paths) {
            RepositoryPath scrPath = repositoryPathResolver.resolve(repository, p);
            Path targetPath = Path.of(prefixPath + p);
            try {
                if(!Files.exists(targetPath)) {
                    Files.createDirectories(targetPath.getParent());
                }
                Files.copy(scrPath, targetPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
            } catch (IOException e) {
                log.info("{}文件备份失败",p);
            }
        }
        String src = genZip(repository,Path.of(prefixPath));
        if( Objects.nonNull(src)) {
            saveZip(repository,src);
        }else {
            log.info("unable to backup path {}", path);
        }
    }


    // 先保存zip文件
    private String genZip(Repository repository,Path targetPath) {
        String dateStr= DateUtil.format(DateUtil.date(), DatePattern.PURE_DATETIME_PATTERN)+".zip";
        String prefixPath=tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId()+"/"+dateStr;
        if(!Files.exists(targetPath)){
            log.info("备份文件不存在");
            return null;
        }
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(prefixPath));Stream<Path> pathStream = Files.walk(targetPath)) {
            pathStream.forEach(path -> {
                        String zipEntryName = targetPath.relativize(path).toString().replace("\\", "/");
                        try {
                            if (Files.isDirectory(path)) {
                                zos.putNextEntry(new ZipEntry(zipEntryName + "/")); // 保证目录结构
                                zos.closeEntry();
                            } else {
                                zos.putNextEntry(new ZipEntry(zipEntryName));
                                Files.copy(path, zos);
                                zos.closeEntry();
                            }
                        } catch (IOException e) {
                            log.info("打包异常");
                        }
                    });
        } catch (Exception e) {
            log.info("操作异常{}", e.getMessage(), e);
            return null;
        }
        return prefixPath;
    }

    void saveZip(Repository repository,String src) {
        // 查找备份的仓库是否存在
        String backupName = repository.getId() + "_backup";
        String storageId = repository.getStorage().getId();
        Repository backupRepo = repository.getStorage().getRepository(backupName);
        try {
            if (Objects.isNull(backupRepo)) {
                //创建一个本地的仓库
                RepositoryDto newRepo = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId())
                        .getRepository(repository.getId());
                newRepo.setId(backupName);
                newRepo.setLayout(RawCoordinates.LAYOUT_NAME);
                newRepo.setType(RepositoryTypeEnum.HOSTED.getType());
                newRepo.setRemoteRepository(null);
                newRepo.setSubLayout("raw");
                newRepo.setAllowsDeployment(true);
                newRepo.setAllowsRedeployment(true);
                newRepo.setProxyConfiguration(null);
                configurationManagementService.saveRepository(newRepo.getStorage().getId(), newRepo);
                backupRepo = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId())
                        .getRepository(backupName);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(backupRepo);
                try {
                    if (!Files.exists(repositoryPath)) {
                        repositoryManagementService.createRepository(storageId, backupRepo.getId());
                    }
                } catch (Exception ex) {
                    log.error("Failed to create the repository path {}!", backupName, ex);
                    try {
                        configurationManagementService.removeRepository(storageId, backupName);
                    } catch (Exception e) {
                        log.error("Failed to remove the repository {}!",  backupRepo.getId(), e);
                    }
                    throw new RuntimeException(ex.getMessage());
                }
            }
            String path=src.substring(src.lastIndexOf("/")+1);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(backupRepo,path );
            artifactManagementService.validateAndStore(repositoryPath, Path.of(src));
        } catch (Exception e) {
            log.error("备份存储异常");
        }
    }
}
