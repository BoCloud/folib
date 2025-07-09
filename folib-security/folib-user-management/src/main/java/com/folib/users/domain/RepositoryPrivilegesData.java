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
package com.folib.users.domain;

import static java.util.stream.Collectors.toSet;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import javax.annotation.concurrent.Immutable;

import com.folib.users.dto.PathPrivilegesDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import com.folib.users.dto.RepositoryPrivileges;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class RepositoryPrivilegesData
        implements Serializable, RepositoryPrivileges
{

    private final String repositoryId;

    private final Set<Privileges> repositoryPrivileges;

    private final Set<PathPrivilegesData> pathPrivileges;

    public RepositoryPrivilegesData(final RepositoryPrivilegesDto delegate)
    {
        this.repositoryId = delegate.getRepositoryId();
        this.repositoryPrivileges = immuteRepositoryPrivileges(delegate.getRepositoryPrivileges());
        this.pathPrivileges = immutePathPrivileges(delegate.getPathPrivileges());
    }

    private Set<Privileges> immuteRepositoryPrivileges(final Set<Privileges> set)
    {
        return set != null ? ImmutableSet.copyOf(set)
                : Collections.emptySet();
    }

    private Set<PathPrivilegesData> immutePathPrivileges(final Set<PathPrivilegesDto> source)
    {
        return source != null ?
               ImmutableSet.copyOf(source.stream().map(PathPrivilegesData::new).collect(toSet())) :
               Collections.emptySet();
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public Set<Privileges> getRepositoryPrivileges()
    {
        return repositoryPrivileges;
    }

    public Set<PathPrivilegesData> getPathPrivileges()
    {
        return pathPrivileges;
    }
}
