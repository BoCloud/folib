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
package com.folib.converters.users;

import com.folib.controllers.users.support.AccessModelOutput;
import com.folib.controllers.users.support.RepositoryAccessModelOutput;
import com.folib.users.domain.AccessModelData;
import com.folib.users.dto.PathPrivileges;
import com.folib.users.dto.RepositoryPrivileges;
import com.folib.users.dto.StoragePrivileges;

import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum AccessModelToAccessModelOutputConverter
        implements Converter<AccessModelData, AccessModelOutput>
{
    INSTANCE;
    
    @Override
    public AccessModelOutput convert(final AccessModelData source)
    {
        if (source == null)
        {
            return null;
        }

        AccessModelOutput result = new AccessModelOutput();
        for (StoragePrivileges storage : source.getStorageAuthorities())
        {
            for (RepositoryPrivileges repository : storage.getRepositoryPrivileges())
            {
                if (CollectionUtils.isNotEmpty(repository.getRepositoryPrivileges()))
                {
                    RepositoryAccessModelOutput repositoryAccess = getRepositoryAccessOrAddNewOne(result,
                                                                                                  storage.getStorageId(),
                                                                                                  repository.getRepositoryId(),
                                                                                                  null,
                                                                                                  false);
                    repositoryAccess.getPrivileges()
                                    .addAll(repository.getRepositoryPrivileges()
                                                      .stream()
                                                      .map(p -> p.name())
                                                      .collect(Collectors.toSet()));
                }
                for (PathPrivileges pathPrivilege : repository.getPathPrivileges())
                {
                    RepositoryAccessModelOutput repositoryAccess = getRepositoryAccessOrAddNewOne(result,
                                                                                                  storage.getStorageId(),
                                                                                                  repository.getRepositoryId(),
                                                                                                  pathPrivilege.getPath(),
                                                                                                  pathPrivilege.isWildcard());

                    repositoryAccess.getPrivileges().addAll(pathPrivilege.getPrivileges()
                                                                         .stream()
                                                                         .map(p -> p.name())
                                                                         .collect(Collectors.toSet()));
                }
            }
        }

        return result;
    }

    private RepositoryAccessModelOutput getRepositoryAccessOrAddNewOne(final AccessModelOutput result,
                                                                       final String storageId,
                                                                       final String repositoryId,
                                                                       final String path,
                                                                       final boolean wildcard)
    {
        return result.getRepositoryAccess(storageId,
                                          repositoryId,
                                          path,
                                          wildcard)
                     .orElseGet(() ->
                                {
                                    RepositoryAccessModelOutput repositoryAccess = new RepositoryAccessModelOutput();
                                    repositoryAccess.setRepositoryId(repositoryId);
                                    repositoryAccess.setStorageId(storageId);
                                    repositoryAccess.setPath(path);
                                    repositoryAccess.setWildcard(wildcard);
                                    result.getRepositoriesAccess().add(repositoryAccess);
                                    return repositoryAccess;
                                });
    }
}
