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

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.users.domain.Privileges;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Veadan
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class StoragePrivilegesDto
        implements Serializable, StoragePrivileges
{

    private Set<RepositoryPrivilegesDto> repositoryPrivileges = new LinkedHashSet<>();

    @JsonProperty(value = "storageId")
    private String storageId;

    private Set<Privileges> storagePrivileges = new LinkedHashSet<>();


    @JsonCreator
    public StoragePrivilegesDto(@JsonProperty(value = "storageId", required = true) String storageId,@JsonProperty(value = "storagePrivileges", required = false) Set<Privileges> storagePrivileges)
    {
        this.storageId = storageId;
        this.storagePrivileges = storagePrivileges;
    }

    @Override
    public Set<RepositoryPrivilegesDto> getRepositoryPrivileges()
    {
        return repositoryPrivileges;
    }

    @Override
    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(final String storageId)
    {
        this.storageId = storageId;
    }

    public Optional<RepositoryPrivilegesDto> getRepositoryPrivileges(final String repositoryId)
    {
        return repositoryPrivileges.stream().filter(r -> r.getRepositoryId().equals(repositoryId)).findFirst();
    }

    @Override
    public Set<Privileges> getStoragePrivileges() {
        return storagePrivileges;
    }
}
