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
package com.folib.layout.providers;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.folib.artifact.coordinates.CargoCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.repository.CargoRepositoryFeatures;
import com.folib.storage.repository.CargoRepositoryStrategy;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.utils.CargoConstants;
import com.folib.utils.CargoUtil;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Component("cargoLayoutProvider")
public class CargoLayoutProvider extends AbstractLayoutProvider<CargoCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CargoLayoutProvider.class);
    public  static final String ALIAS = CargoCoordinates.LAYOUT_NAME;
    @Lazy
    @Inject
    private CargoRepositoryStrategy cargoRepositoryManagementStrategy;
    @Lazy
    @Inject
    private CargoRepositoryFeatures cargoRepositoryFeatures;
    @Lazy
    @Inject
    protected ArtifactManagementService artifactManagementService;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;

    @Lazy
    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return cargoRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().endsWith(CargoConstants.CRATE_SUFFIX) || repositoryPath.getPath().startsWith("index")) {
            return false;
        }
        return true;
    }

    private boolean isArtifact(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().endsWith(CargoConstants.CRATE_SUFFIX) || repositoryPath.getPath().startsWith("crates")) {
            return true;
        }
        return false;
    }

    private boolean isIndex(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().startsWith("index")) {
            return true;
        }
        return false;
    }

    @Override
    public CargoCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new CargoCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    public CargoRepositoryStrategy getRepositoryManagementStrategy() {
        return cargoRepositoryManagementStrategy;
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
        Map<RepositoryFileAttributeType, Object> result =  super.getRepositoryFileAttributes(repositoryPath, attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && isArtifact(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                case METADATA:
                    value = (Boolean) value && isArtifactMetadata(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isIndex(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo);

                    result.put(attributeType, value);

                    break;
            }
        }
        return result;
    }

    @Override
    public void targetUrl(RepositoryPath path) throws IOException {
        if (Objects.isNull(path) || StringUtils.isBlank(path.getTargetUrl())) {
            return;
        }
        Repository repository = path.getRepository();
        if (!RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            return;
        }
        String remoteUrl = null;
        if(path.getTargetUrl().endsWith("/download")){
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(path.getStorageId(), path.getRepositoryId(), ".proxy-config.json");
            JSONObject jsonObject = CargoUtil.getMapper().readValue(Files.readString(repositoryPath), JSONObject.class);
            remoteUrl = jsonObject.getString("dl");
        }else {
            remoteUrl = repository.getRemoteRepository().getUrl();
        }
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

    @Override
    public void initData(String storageId, String repositoryId) {
        try {
            Repository repository = configurationManager.getConfiguration().getStorage(storageId).getRepository(repositoryId);
            if(repository.isProxyRepository()){
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".proxy-config.json");
                if (Files.exists(repositoryPath)) {
                    Files.deleteIfExists(repositoryPath);
                }
                repositoryPath.setTargetUrl("config.json");
                try {
                    repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), "config.json");
            if (Files.exists(repositoryPath)) {
                Files.deleteIfExists(repositoryPath);
            }
            String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
            ObjectMapper objectMapper = new ObjectMapper();
            Map<String, String> map = new HashMap<>();
            map.put("dl", String.format("%s/storages/%s/%s/api/v1/crates", baseUrl, repository.getStorage().getId(), repository.getId()));
            map.put("api", String.format("%s/storages/%s/%s", baseUrl, repository.getStorage().getId(), repository.getId()));
            if (!repository.isAllowAnonymous()) {
                map.put("auth-required", "true");
            }
            objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
            ObjectWriter writer = objectMapper.writerWithDefaultPrettyPrinter();
            //repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            artifactManagementService.store(repositoryPath, new ByteArrayInputStream(writer.writeValueAsString(map).getBytes()));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


    }


}
