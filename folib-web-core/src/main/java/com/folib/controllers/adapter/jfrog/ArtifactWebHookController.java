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
package com.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.folib.components.webhook.WebhookEventsProvider;
import com.folib.components.webhook.WebhookEventsProviderRegistry;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.folib.entity.Dict;
import com.folib.enums.JFrogEventTypeEnum;
import com.folib.enums.WebhookEventsTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.security.exceptions.ExpiredTokenException;
import com.folib.security.exceptions.InvalidTokenException;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.JfrogMigrateService;
import com.folib.storage.Storage;
import com.folib.users.security.SecurityTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/2/26
 **/
@Slf4j
@RequestMapping("/artifactory")
@RestController
public class ArtifactWebHookController {

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Resource
    private JfrogMigrateService jfrogMigrateService;

    @PostMapping("/webhook")
    public ResponseEntity<Object> webhook(@RequestBody String data, HttpServletRequest request) {
        log.info("JFrog event data [{}]", data);
        try {
            WebhookDto webhookDto = JSONObject.parseObject(data, WebhookDto.class);
            String tokenKey = "X-jfrog-event-auth";
            String token = request.getHeader(tokenKey);
            log.info("JFrog event header token [{}] [{}]", tokenKey, token);
            if (StringUtils.isBlank(token)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The header parameter [%s] is required", tokenKey));
            }
            String repositoryKey = "X-repository";
            String repositoryHeader = request.getHeader(repositoryKey);
            // 存储空间固定且仓库同名
            String storageKey = "X-storage";
            String storageHeader = request.getHeader(storageKey);
            if (StringUtils.isBlank(storageHeader) && StringUtils.isBlank(repositoryHeader)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The either header parameter [%s] or [%s] must be passed", repositoryKey, storageKey));
            }
            securityTokenProvider.getClaims(token, true);
            if (!JFrogEventTypeEnum.needHandle(webhookDto.getEventType())) {
                log.info("JFrog event [{}] not need handle", webhookDto.getEventType());
                return ResponseEntity.ok("");
            }
            log.info("JFrog event header storage [{}] [{}] header repository [{}] [{}]", storageKey, storageHeader, repositoryKey, repositoryHeader);
            String storageId = "", repositoryId = "";
            if (StringUtils.isNotBlank(repositoryHeader)) {
                //固定仓库
                storageId = ConfigurationUtils.getStorageId(repositoryHeader, repositoryHeader);
                repositoryId = ConfigurationUtils.getRepositoryId(repositoryHeader);
            } else if (StringUtils.isNotBlank(storageHeader)) {
                //固定存储空间下的同名仓库
                storageId = storageHeader;
                repositoryId = webhookDto.getData().getRepoKey();
            }
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                log.warn("JFrog event storage [{}] not found", storageId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                log.warn("JFrog event storage [{}] repository [{}] not found", storageId, repositoryId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
            }
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            ArtifactData artifactData = webhookDto.getData();
            RepositoryPath repositoryPath = rootRepositoryPath.resolve(artifactData.getPath());
            Dict dict = jfrogMigrateService.getWebhookSetting();
            if (Objects.isNull(dict)) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Cannot find JFrog artifact migrate info");
            }
            boolean exists = false;
            String currentDigest = "";
            if (JFrogEventTypeEnum.DEPLOYED.getType().equalsIgnoreCase(webhookDto.getEventType()) && StringUtils.isNotBlank(artifactData.getSha256()) && Files.exists(repositoryPath)) {
                LayoutFileSystemProvider provider = rootRepositoryPath.getFileSystem().provider();
                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, MessageDigestAlgorithms.SHA_256);
                if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                    try {
                        currentDigest = Files.readString(checksumPath);
                    } catch (IOException e) {
                        log.error(ExceptionUtils.getStackTrace(e));
                    }
                    exists = artifactData.getSha256().equals(currentDigest);
                }
            }
            if (exists) {
                log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] exists skip sync", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
                return ResponseEntity.ok("");
            }
            log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] not exists", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
            WebhookEventsProvider webhookEventsProvider = webhookEventsProviderRegistry.getProvider(WebhookEventsTypeEnum.resolveType(repositoryPath.getRepository().getLayout()));
            boolean result = webhookEventsProvider.handler(webhookDto, repositoryPath, dict, 1);
            if (!result) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(String.format("Handle event error [%s]", data));
            }
            return ResponseEntity.ok("");
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            if (ex instanceof ExpiredTokenException) {
                log.warn("JFrog event the token has expired");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token has expired");
            } else if (ex instanceof InvalidTokenException) {
                log.warn("JFrog event the token is invalid");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token is invalid");
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(String.format("Handle event error [%s]", data));
        }
    }

}
