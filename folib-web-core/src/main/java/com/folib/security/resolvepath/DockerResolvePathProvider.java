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

import com.folib.constant.GlobalConstants;
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
public class DockerResolvePathProvider implements ResolvePathProvider {

    public static final String MANIFESTS = "/manifests/";

    public static final String BLOBS = "/blobs/";

    public static final String V2 = "/v2/";

    public static final String STORAGES = "/storages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.DOCKER.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.DOCKER.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.startsWith(V2) || relativePath.startsWith(STORAGES)) {
            if (relativePath.startsWith(V2)) {
                relativePath = relativePath.replaceFirst(V2, STORAGES);
            }
            if (relativePath.contains(MANIFESTS)) {
                if (!relativePath.contains(GlobalConstants.SHA_256)) {
                    //tag号开头的
                    relativePath = relativePath.replace(MANIFESTS, "/");
                } else {
                    //sha256 manifest文件
                    relativePath = relativePath.replace("manifests/", "manifest/");
                }
            }
        } else {
            if (relativePath.contains(MANIFESTS)) {
                if (!relativePath.contains(GlobalConstants.SHA_256)) {
                    //tag号开头的
                    relativePath = relativePath.replace(MANIFESTS, "/");
                } else {
                    //sha256 manifest文件
                    relativePath = relativePath.substring(relativePath.indexOf("manifests/")).replace("manifests/", "manifest/");
                }
            } else if (relativePath.contains(BLOBS)) {
                //sha256 blob文件
                relativePath = relativePath.substring(relativePath.indexOf("blobs/"));
            }
        }
        return relativePath;
    }
}
