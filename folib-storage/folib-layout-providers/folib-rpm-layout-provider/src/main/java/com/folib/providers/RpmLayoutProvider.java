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
package com.folib.providers;

import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.metadata.indexer.RpmGroupRepoIndexer;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.RepositoryStrategy;
import com.folib.repository.RpmRepositoryFeatures;
import com.folib.repository.RpmRepositoryStrategy;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 */
@Component
public class RpmLayoutProvider extends AbstractLayoutProvider<RpmCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(RpmLayoutProvider.class);

    public static final String ALIAS = RpmCoordinates.LAYOUT_NAME;

    public static final String USER_AGENT_PREFIX =ALIAS;

//    public static final String RPM_USER_PATH = "-/user/org.couchdb.user:";

//    public static final Pattern RPM_URL_USERNAME_PATTERN = Pattern.compile(
//            "(?:" + RpmLayoutProvider.RPM_USER_PATH + ")(.*)");


    @Lazy
    @Inject
    private RpmRepositoryStrategy rpmRepositoryManagementStrategy;
    @Lazy
    @Inject
    private RpmRepositoryFeatures rpmRepositoryFeatures;
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Value("${folib.temp}")
    private  String tempPath;

    @PostConstruct
    public void register()
    {
       // headerMappingRegistry.register(ALIAS, USER_AGENT_PREFIX);
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public RpmCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException    {
        return RpmCoordinates.parse(RepositoryFiles.relativizePath(path));
    }


    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {return false;}

    public boolean isRpmMetadata(RepositoryPath path)
    {
        return !path.getFileName().toString().endsWith(".rpm");
       // return true;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        if (attributeTypes == null || attributeTypes.length == 0) {
            return super.getRepositoryFileAttributes(repositoryPath, attributeTypes);
        }

        Map<RepositoryFileAttributeType, Object> result = new ConcurrentHashMap<>(super.getRepositoryFileAttributes(repositoryPath, attributeTypes));

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);

            switch (attributeType) {
                case ARTIFACT:
                    if (value instanceof Boolean) {
                        boolean artifactValue = (Boolean) value && !isRpmMetadata(repositoryPath);
                        result.put(attributeType, artifactValue);
                    }
                    break;
                case METADATA:
                    if (value instanceof Boolean) {
                        boolean metadataValue = (Boolean) value || isRpmMetadata(repositoryPath);
                        result.put(attributeType, metadataValue);
                    }
                    break;
                case REFRESH_CONTENT:
                    try {
                        if (value instanceof Boolean) {
                            Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                            boolean refreshContentValue = BooleanUtils.isTrue((Boolean) value) ||
                                    (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isIndex(repositoryPath)) &&
                                            !RepositoryFiles.wasModifiedAfter(repositoryPath, halfAnHourAgo);
                            result.put(attributeType, refreshContentValue);
                        }
                    } catch (Exception e) {
                        // Log the exception or handle it appropriately
                        logger.error("Error processing REFRESH_CONTENT attribute", e);
                        throw new IOException("Error processing REFRESH_CONTENT attribute", e);
                    }
                    break;
                default:
                    break;
            }
        }

        return result;
    }


    @Override
    public RepositoryStrategy getRepositoryManagementStrategy()
    {
        return rpmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return rpmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    private boolean isIndex(RepositoryPath repositoryPath) {
        if (repositoryPath == null || repositoryPath.getPath() == null || repositoryPath.getPath().isEmpty()) {
            return false;
        }
        String path = repositoryPath.getPath();
        return path.endsWith(".xml") || path.endsWith(".xml.gz");
    }



    @Override
    public void initData(String storageId, String repositoryId) {
        logger.info(" rpm repository initData storageId:{} repositoryId:{}", storageId,repositoryId);
        // 获取存储配置时添加空指针检查
        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        if (storage == null) {
            throw new IllegalStateException("Storage not found: " + storageId);
        }

        // 获取仓库时添加空指针检查
        Repository repository = storage.getRepository(repositoryId);
        if (repository == null) {
            throw new IllegalStateException("Repository not found: " + repositoryId);
        }

        // 提前返回条件判断保持原逻辑
        if (!"group".equals(repository.getType())) {
            return;
        }
        // 创建索引器（保持原有实例化方式，如有需要可考虑依赖注入）
        RpmGroupRepoIndexer indexer = new RpmGroupRepoIndexer(
                tempPath,
                repositoryPathResolver,
                artifactManagementService,
                configurationManager
        );
        CompletableFuture<Void> idexerFuture = CompletableFuture.runAsync(() -> {
            try {
                indexer.aggregationIndexer(repository);
            } catch (Exception e) {
                // 记录错误日志并保留原始异常信息
                logger.error("Failed to index repository {}/{}", storageId, repositoryId, e);
                throw new RuntimeException("Indexing failed", e);
            }
        }).exceptionally(ex -> {
            if (ex.getCause() != null) {
                logger.error("rpm 组合库构建索引异常", ex.getCause()); // 记录完整的异常堆栈信息
            } else {
                logger.error("rpm 组合库构建索引异常", ex); // 记录完整的异常堆栈信息
            }
            return null;
        });
    }

}
