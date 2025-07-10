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
package com.folib.security.resolvepath;

import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class NpmResolvePathProvider implements ResolvePathProvider {

    public static final String BINARY = "-/binary/";

    public static final String STORAGE = "/storages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.NPM.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.NPM.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        String extension = FilenameUtils.getExtension(relativePath);
        if (!relativePath.startsWith(STORAGE) && !relativePath.startsWith(BINARY) && StringUtils.isNotBlank(extension) && NpmCoordinates.NPM_EXTENSION_PATTERN.matcher(extension).matches()) {
            NpmCoordinates npmArtifactCoordinates = NpmCoordinates.parseByResolvePath(relativePath);
            if (Objects.nonNull(npmArtifactCoordinates)) {
                relativePath = npmArtifactCoordinates.buildPath();
            }
        }
        return relativePath;
    }
}
