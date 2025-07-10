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
package com.folib.providers.layout;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ArtifactGroup;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.RepositoryStrategy;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Set;

/**
 * @author Veadan
 */
public interface LayoutProvider<T extends ArtifactCoordinates> {
    RepositoryStrategy getRepositoryManagementStrategy();

    @Nonnull
    Set<String> listArchiveFilenames(RepositoryPath repositoryPath);

    byte[] getContentByFileName(RepositoryPath repositoryPath, String fileName);

    byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName);

    byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName);

    Set<String> getDefaultArtifactCoordinateValidators();

    String getAlias();

    @Nonnull
    Set<ArtifactGroup> getArtifactGroups(RepositoryPath path)
            throws IOException;

    default void initData(String storageId, String repositoryId) {

    }

    default void targetUrl(RepositoryPath path) throws IOException {
        if (Objects.isNull(path) || StringUtils.isBlank(path.getTargetUrl())) {
            return;
        }
        Repository repository = path.getRepository();
        if (!RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            return;
        }
        if (GlobalConstants.HTTP_PREFIX_LIST.stream().anyMatch(item -> path.getTargetUrl().startsWith(item))) {
            return;
        }
        String remoteUrl = repository.getRemoteRepository().getUrl();
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

    default int refreshContentInterval(RepositoryPath repositoryPath) {
        return GlobalConstants.DEFAULT_REFRESH_CONTENT_INTERVAL;
    }
}
