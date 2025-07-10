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

import java.util.Collection;
import java.util.stream.Collectors;

import com.folib.users.dto.AccessModelDto;
import com.folib.forms.users.AccessModelForm;
import com.folib.forms.users.RepositoryAccessModelForm;
import org.apache.commons.lang3.StringUtils;
import com.folib.users.domain.Privileges;
import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.StoragePrivilegesDto;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public enum AccessModelFormToUserAccessModelDtoConverter
        implements Converter<AccessModelForm, AccessModelDto>
{

    INSTANCE;

    @Override
    public AccessModelDto convert(AccessModelForm accessModelForm)
    {
        if (accessModelForm == null)
        {
            return null;
        }
        
        AccessModelDto userAccessModelDto = new AccessModelDto();
        accessModelForm.getApiAccess()
                       .stream()
                       .map(p -> Privileges.valueOf(p))
                       .forEach(p -> userAccessModelDto.getApiAuthorities().add(p));
        
        for (RepositoryAccessModelForm repositoryAccess : accessModelForm.getRepositoriesAccess())
        {
            StoragePrivilegesDto storage = userAccessModelDto.getStorageAuthorities(repositoryAccess.getStorageId())
                                                       .orElseGet(
                                                               () ->
                                                               {
                                                                   StoragePrivilegesDto userStorageDto = new StoragePrivilegesDto();
                                                                   userStorageDto.setStorageId(
                                                                           repositoryAccess.getStorageId());
                                                                   if (StringUtils.isBlank(repositoryAccess.getRepositoryId())) {
                                                                       userStorageDto.getStoragePrivileges().addAll(pullPrivileges(repositoryAccess));
                                                                   }
                                                                   userAccessModelDto.getStorageAuthorities().add(userStorageDto);
                                                                   return userStorageDto;
                                                               });

            if (StringUtils.isBlank(repositoryAccess.getRepositoryId())) {
                continue;
            }
            RepositoryPrivilegesDto repository = storage.getRepositoryPrivileges(repositoryAccess.getRepositoryId())
                                                  .orElseGet(
                                                          () ->
                                                          {
                                                              RepositoryPrivilegesDto userRepositoryDto = new RepositoryPrivilegesDto();
                                                              userRepositoryDto.setRepositoryId(
                                                                      repositoryAccess.getRepositoryId());
                                                              storage.getRepositoryPrivileges().add(userRepositoryDto);
                                                              return userRepositoryDto;
                                                          });

            if (StringUtils.isBlank(repositoryAccess.getPath()))
            {
                repository.getRepositoryPrivileges().addAll(pullPrivileges(repositoryAccess));
                continue;
            }

            PathPrivilegesDto pathPrivileges = repository.getPathPrivilege(repositoryAccess.getPath(),
                                                                           repositoryAccess.isWildcard())
                                                         .orElseGet(
                                                                    () -> {
                                                                        PathPrivilegesDto pathPrivilegesDto = new PathPrivilegesDto();
                                                                        pathPrivilegesDto.setPath(
                                                                                                  repositoryAccess.getPath());
                                                                        pathPrivilegesDto.setWildcard(
                                                                                                      repositoryAccess.isWildcard());
                                                                        repository.getPathPrivileges()
                                                                                  .add(
                                                                                       pathPrivilegesDto);
                                                                        return pathPrivilegesDto;
                                                                    });
            pathPrivileges.getPrivileges().addAll(pullPrivileges(repositoryAccess));

        }
        return userAccessModelDto;
    }

    private Collection<Privileges> pullPrivileges(final RepositoryAccessModelForm repositoryAccess)
    {
        return repositoryAccess.getPrivileges()
                               .stream()
                               .map(p -> Privileges.valueOf(p))
                               .collect(Collectors.toSet());
    }
}
