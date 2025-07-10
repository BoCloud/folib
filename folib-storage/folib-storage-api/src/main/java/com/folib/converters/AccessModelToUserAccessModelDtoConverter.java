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
package com.folib.converters;

import com.folib.domain.AccessModel;
import com.folib.domain.RepositoryAccessModel;
import com.folib.users.domain.Privileges;
import com.folib.users.dto.AccessModelDto;
import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.StoragePrivilegesDto;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
public class AccessModelToUserAccessModelDtoConverter {

    public static AccessModelDto convert(AccessModel accessModel) {
        if (accessModel == null) {
            return null;
        }
        AccessModelDto userAccessModelDto = new AccessModelDto();
        accessModel.getApiAccess()
                .stream()
                .map(p -> Privileges.valueOf(p))
                .forEach(p -> userAccessModelDto.getApiAuthorities().add(p));
        for (RepositoryAccessModel repositoryAccessModel : accessModel.getRepositoriesAccess()) {
            StoragePrivilegesDto storage = userAccessModelDto.getStorageAuthorities(repositoryAccessModel.getStorageId())
                    .orElseGet(
                            () ->
                            {
                                StoragePrivilegesDto userStorageDto = new StoragePrivilegesDto();
                                userStorageDto.setStorageId(
                                        repositoryAccessModel.getStorageId());
                                if (StringUtils.isBlank(repositoryAccessModel.getRepositoryId())) {
                                    userStorageDto.getStoragePrivileges().addAll(pullPrivileges(repositoryAccessModel));
                                }
                                userAccessModelDto.getStorageAuthorities().add(userStorageDto);
                                return userStorageDto;
                            });
            if (StringUtils.isBlank(repositoryAccessModel.getRepositoryId())) {
                continue;
            }
            RepositoryPrivilegesDto repository = storage.getRepositoryPrivileges(repositoryAccessModel.getRepositoryId())
                    .orElseGet(
                            () ->
                            {
                                RepositoryPrivilegesDto userRepositoryDto = new RepositoryPrivilegesDto();
                                userRepositoryDto.setRepositoryId(
                                        repositoryAccessModel.getRepositoryId());
                                storage.getRepositoryPrivileges().add(userRepositoryDto);
                                return userRepositoryDto;
                            });
            if (StringUtils.isBlank(repositoryAccessModel.getPath())) {
                repository.getRepositoryPrivileges().addAll(pullPrivileges(repositoryAccessModel));
                continue;
            }

            PathPrivilegesDto pathPrivileges = repository.getPathPrivilege(repositoryAccessModel.getPath(),
                    repositoryAccessModel.isWildcard())
                    .orElseGet(
                            () -> {
                                PathPrivilegesDto pathPrivilegesDto = new PathPrivilegesDto();
                                pathPrivilegesDto.setPath(
                                        repositoryAccessModel.getPath());
                                pathPrivilegesDto.setWildcard(
                                        repositoryAccessModel.isWildcard());
                                repository.getPathPrivileges()
                                        .add(
                                                pathPrivilegesDto);
                                return pathPrivilegesDto;
                            });
            pathPrivileges.getPrivileges().addAll(pullPrivileges(repositoryAccessModel));
        }
        return userAccessModelDto;
    }

    private static Collection<Privileges> pullPrivileges(final RepositoryAccessModel repositoryAccessModel) {
        return repositoryAccessModel.getPrivileges()
                .stream()
                .map(p -> Privileges.valueOf(p))
                .collect(Collectors.toSet());
    }
}
