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
package com.folib.users.dto;


import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.folib.users.domain.AccessModelData;
import com.folib.users.domain.Privileges;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Veadan
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AccessModelDto
        implements AccessModel
{

    private Set<Privileges> apiAuthorities = new LinkedHashSet<>();
    
    private Set<StoragePrivilegesDto> storageAuthorities = new LinkedHashSet<>();


    @Override
    public Set<Privileges> getApiAuthorities()
    {
        return apiAuthorities;
    }

    public Set<StoragePrivilegesDto> getStorageAuthorities()
    {
        return storageAuthorities;
    }

    public Optional<StoragePrivilegesDto> getStorageAuthorities(final String storageId)
    {
        return storageAuthorities.stream().filter(s -> s.getStorageId().equals(storageId)).findFirst();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return AccessModelData.getPathAuthorities(url, storageAuthorities, false);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String path, boolean enableSplitPath) {
        return AccessModelData.getPathAuthorities(path, storageAuthorities, enableSplitPath);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths, boolean enableSplitPath) {
        return AccessModelData.getPathAuthorities(storageId, repositoryId, paths, storageAuthorities, enableSplitPath);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return AccessModelData.getPathAuthorities(storageId, repositoryId, paths, storageAuthorities, false);
    }

}
