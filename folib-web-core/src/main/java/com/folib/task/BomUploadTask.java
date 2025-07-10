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
import com.alibaba.fastjson.JSONObject;
import com.folib.components.DistributedLockComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.folib.components.thirdparty.foeyes.enums.UploadStatusEnum;
import com.folib.domain.bom.Bom;
import com.folib.domain.bom.FoEyes;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.scanner.entity.ScanRules;
import com.folib.scanner.service.ScanRulesService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;


import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 * bom重传task
 */
@Slf4j
@Component
@EnableScheduling
public class BomUploadTask {

    @Inject
    @Lazy
    private ScanRulesService scanRulesService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private FoEyesComponent foEyesComponent;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        log.info("BomUploadTask starting time [{}]", DateUtil.now());
        if (!foEyesComponent.enable()) {
            log.info("foEyes not enable bomUploadTask skip...");
            return;
        }
        String lockName = "BomUploadTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                List<ScanRules> scanRulesList = scanRulesService.queryBomOnScanList();
                if (CollectionUtils.isEmpty(scanRulesList)) {
                    return;
                }
                for (ScanRules scanRules : scanRulesList) {
                    try {
                        RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(scanRules.getStorage(), scanRules.getRepository());
                        try (Stream<Path> pathStream = Files.walk(rootRepositoryPath)) {
                            pathStream.filter(item -> {
                                try {
                                    RepositoryPath itemRepositoryPath = (RepositoryPath) item;
                                    return !RepositoryFiles.isHidden(itemRepositoryPath) && RepositoryFiles.isArtifact(itemRepositoryPath) && Files.exists(artifactComponent.getBomRepositoryPath(itemRepositoryPath));
                                } catch (Exception ex) {
                                    log.error("Bom upload handler path [{}] error [{}]", item.toString(), ExceptionUtils.getStackTrace(ex));
                                }
                                return false;
                            }).forEach(path -> {
                                try {
                                    RepositoryPath repositoryPath = (RepositoryPath) path;
                                    RepositoryPath bomRepositoryPath = artifactComponent.getBomRepositoryPath(repositoryPath);
                                    if (Files.exists(bomRepositoryPath)) {
                                        Bom bom = JSONObject.parseObject(Files.readString(bomRepositoryPath), Bom.class);
                                        if (Objects.nonNull(bom)) {
                                            FoEyes foEyes = bom.getFoEyes();
                                            boolean upload = Objects.isNull(foEyes) || !UploadStatusEnum.UPLOAD_SUCCESS.getType().equals(foEyes.getUploadStatus());
                                            if (upload) {
                                                //将SBOM传给foeyes
                                                foEyesComponent.bomUpload(repositoryPath, bomRepositoryPath, bom);
                                            }
                                        }
                                    }
                                } catch (Exception ex) {
                                    log.error("Bom upload handler path [{}] error [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
                                }
                            });
                        }
                    } catch (Exception ex) {
                        log.error("Bom upload handler storageId [{}] repositoryId [{}] error [{}]", scanRules.getStorage(), scanRules.getRepository(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("BomUploadTask end time [{}]", DateUtil.now());
    }
}
