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
package com.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.security.SecurityComponent;
import com.folib.dto.ArtifactDto;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Objects;
import java.util.concurrent.Callable;

@Slf4j
public class PullArtifactTask implements Callable<String> {

    private String path;
    private String srcUrl;
    private String targetStorageId;
    private String targetRepostoryId;
    private RepositoryPathResolver repositoryPathResolver;
    private ArtifactManagementService artifactManagementService;
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;
    private ArtifactDto artifac;
    private String metaData;
    private PromotionUtil promotionUtil;
    private SecurityComponent securityComponent;

    public PullArtifactTask(String path, String srcUrl, String targetStorageId, String targetRepostoryId,
                            RepositoryPathResolver repositoryPathResolver,
                            ArtifactManagementService artifactManagementService,
                            ProxyRepositoryConnectionPoolConfigurationService clientPool,
                            PromotionUtil promotionUtil,
                            ArtifactDto artifac,
                            String metaData) {
        this.path = path;
        this.srcUrl = srcUrl;
        this.targetStorageId = targetStorageId;
        this.targetRepostoryId = targetRepostoryId;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactManagementService = artifactManagementService;
        this.clientPool = clientPool;
        this.artifac = artifac;
        this.metaData = metaData;
        this.promotionUtil = promotionUtil;
        this.securityComponent = SpringUtil.getBean(SecurityComponent.class);
    }

    @Override
    public String call() throws Exception {
        Response response = null;
        try {
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            if (RepositoryFiles.isChecksum(destPath)) {
                return "ok";
            }
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !DockerCoordinates.exclude(path)) {
                    try (InputStream is = response.readEntity(InputStream.class);) {
                        Files.copy(is, destPath);
                    }
                    return "ok";
                }
            }
            try (InputStream is = response.readEntity(InputStream.class);) {
                promotionUtil.setMetaData(destPath, metaData);
                artifactManagementService.store(destPath, is);
            }
        } catch (Exception e) {
            // 添加重试机制
            log.error("{} pull error {}", JSON.toJSONString(artifac), e.getMessage());
            boolean rePullResultFlag = false;
            for (int i = 0; i < 5; i++) {
                rePullResultFlag = reTryPull();
                if (rePullResultFlag) {
                    break;
                }
                Thread.sleep(1000L);
            }
            if (!rePullResultFlag) {
                throw new Exception(e.getMessage());
            }
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        log.info("File {} pulled", JSON.toJSONString(artifac));
        return "ok";
    }

    private boolean reTryPull() {
        Response response = null;
        try {
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(srcUrl);
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.post(Entity.entity(artifac, MediaType.APPLICATION_JSON));
            RepositoryPath destPath = repositoryPathResolver.resolve(targetStorageId, targetRepostoryId, path);
            boolean isDocker = destPath.getRepository().getLayout().equalsIgnoreCase("docker");
            if (isDocker) {
                if (!path.contains("sha256") && !DockerCoordinates.exclude(path)) {
                    try (InputStream is = response.readEntity(InputStream.class);) {
                        Files.copy(is, destPath);
                    }
                    return true;
                }
            }

            try (InputStream is = response.readEntity(InputStream.class);) {
                promotionUtil.setMetaData(destPath, metaData);
                artifactManagementService.store(destPath, is);
            }
            return true;
        } catch (Exception e) {
            return false;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }
}
