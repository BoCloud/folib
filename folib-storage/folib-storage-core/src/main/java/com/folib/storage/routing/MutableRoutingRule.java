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
package com.folib.storage.routing;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableRoutingRule
        implements Serializable
{

    private UUID uuid;

    private String storageId;

    private String groupRepositoryId;

    private String pattern;

    private String type;

    private List<MutableRoutingRuleRepository> repositories = new ArrayList<>();

    public UUID getUuid()
    {
        return uuid;
    }

    public void setUuid(UUID uuid)
    {
        this.uuid = uuid;
    }

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

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

    public List<MutableRoutingRuleRepository> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(List<MutableRoutingRuleRepository> repositories)
    {
        this.repositories = repositories;
    }

    public static MutableRoutingRule create(String groupStorageId,
                                            String groupRepositoryId,
                                            List<MutableRoutingRuleRepository> repositories,
                                            String rulePattern,
                                            RoutingRuleTypeEnum type)
    {
        MutableRoutingRule routingRule = new MutableRoutingRule();
        routingRule.setPattern(rulePattern);
        routingRule.setStorageId(groupStorageId);
        routingRule.setGroupRepositoryId(groupRepositoryId);
        routingRule.setType(type.getType());
        routingRule.setRepositories(repositories);
        return routingRule;
    }

    public boolean updateBy(MutableRoutingRule routingRule)
    {
        this.setRepositories(routingRule.getRepositories());
        this.setPattern(routingRule.getPattern());
        this.setStorageId(routingRule.getStorageId());
        this.setGroupRepositoryId(routingRule.getGroupRepositoryId());
        this.setType(routingRule.getType());
        return true;
    }
}
