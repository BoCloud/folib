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
package com.folib.forms.routing;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.storage.routing.RoutingRuleTypeEnum;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Veadan
 * @author veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoutingRuleForm
{

    private String storageId;

    private String groupRepositoryId;

    @NotBlank(message = "A pattern must be specified.")
    private String pattern;

    @NotNull(message = "A type must be specified.")
    private RoutingRuleTypeEnum type;

    @Valid
    private List<RoutingRuleRepositoryForm> repositories = new ArrayList<>();

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getGroupRepositoryId()
    {
        return groupRepositoryId;
    }

    public void setGroupRepositoryId(String groupRepositoryId)
    {
        this.groupRepositoryId = groupRepositoryId;
    }

    public String getPattern()
    {
        return pattern;
    }

    public void setPattern(String pattern)
    {
        this.pattern = pattern;
    }

    public RoutingRuleTypeEnum getType()
    {
        return type;
    }

    public void setType(RoutingRuleTypeEnum type)
    {
        this.type = type;
    }

    public List<RoutingRuleRepositoryForm> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(List<RoutingRuleRepositoryForm> repositories)
    {
        this.repositories = repositories;
    }
}
