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
package com.folib.eventlistener.backup;

import com.folib.components.artifact.ArtifactComponent;
import com.folib.entity.Dict;
import com.folib.enums.DictTypeEnum;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * 事件监听，处理备份
 **/
@Slf4j
//@Component
@Deprecated
public class ArtifactEventBackupListener {

    @Inject
    private DictService dictService;

    @Inject
    private ArtifactComponent artifactComponent;

//    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("{} 监听到制品事件：{}，path路径：{}", ArtifactEventBackupListener.class.getSimpleName(), artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return;
        }
        if (validateArtifactEvent(artifactEventTypeEnum) && artifactExists(repositoryPath)) {
            try {
                String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
                Dict dict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.BACKUP_SETTINGS.getType()).dictKey(String.format("%s:%s", storageId, repositoryId)).build());
                if (Objects.nonNull(dict) && StringUtils.isNotBlank(dict.getDictValue())) {
                    String backupDir = dict.getDictValue();
                    Path backupPath = Files.createDirectories(Paths.get(backupDir));
                    String sourcePath = repositoryPath.toString();
                    String prefix = String.format("/%s/%s/", storageId, repositoryId);
                    String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
                    Path targetPath = backupPath.resolve(targetSubPath);
                    log.info("StorageId [{}] repositoryId [{}] 开启备份功能，源制品地址 [{}] 备份制品地址 [{}]", storageId, repositoryId, sourcePath, targetPath.toString());
                    Files.createDirectories(targetPath.getParent());
                    Files.copy(repositoryPath.getTarget(), targetPath, StandardCopyOption.REPLACE_EXISTING);
                    artifactComponent.storeArtifactMetadataFile(repositoryPath, targetPath);
                }
            } catch (Exception ex) {
                log.error("事件监听，处理backup，事件类型：{} repositoryPath：{} 错误：{}", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 制品存在判断
     *
     * @param repositoryPath 制品对象
     * @return true 存在 false 不存在
     */
    public boolean artifactExists(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.error("RepositoryPath [{}] does not exist", repositoryPath);
            return false;
        }
        return true;
    }

}
