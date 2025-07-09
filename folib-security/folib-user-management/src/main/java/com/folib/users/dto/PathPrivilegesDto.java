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
import java.util.Set;

import com.folib.users.domain.Privileges;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * @author veadan
 *
 */
@Data
@Builder
@AllArgsConstructor
public class PathPrivilegesDto
        implements Serializable, PathPrivileges
{

    private String path;

    private boolean wildcard;

    private Set<Privileges> privileges = new LinkedHashSet<>();

    public PathPrivilegesDto()
    {
    }

    @JsonCreator
    public PathPrivilegesDto(@JsonProperty(value = "path", required = true) String path)
    {
        this.path = path;
    }

    public String getPath()
    {
        return path;
    }

    public void setPath(final String path)
    {
        this.path = path;
    }

    public boolean isWildcard()
    {
        return wildcard;
    }

    public void setWildcard(final boolean wildcard)
    {
        this.wildcard = wildcard;
    }

    public Set<Privileges> getPrivileges()
    {
        return privileges;
    }

    public void setPrivileges(Set<Privileges> privileges)
    {
        this.privileges = privileges;
    }
}
