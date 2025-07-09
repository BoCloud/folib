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

import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiResolvePathProvider implements ResolvePathProvider {

    public static final String SIMPLE = "simple/";

    public static final String PACKAGES = "packages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.PYPI.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.PYPI.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.startsWith(SIMPLE)) {
            relativePath = "";
        } else if (relativePath.startsWith(PACKAGES)) {
            relativePath = relativePath.replace("packages/", "");
            PypiCoordinates coordinates;
            try {
                coordinates = PypiCoordinates.parse(relativePath);
                relativePath = coordinates.buildPath();
            } catch (Exception e) {
                log.error("Invalid package name - {}", e.getMessage());
            }
        }
        return relativePath;
    }
}
