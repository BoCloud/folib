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

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableList;
import org.apache.commons.lang3.StringUtils;

/**
 * @author veadan
 */
@Immutable
public class RoutingRule
        implements RepositoryIdentifiable
{

    private final UUID uuid;

    private final String storageId;

    private final String groupRepositoryId;

    private final String pattern;

    private final Pattern regex;

    private final RoutingRuleTypeEnum type;

    private final List<RoutingRuleRepository> repositories;

    public RoutingRule(final MutableRoutingRule delegate)
    {
        this.uuid = delegate.getUuid();
        this.groupRepositoryId = delegate.getGroupRepositoryId();
        this.storageId = delegate.getStorageId();
        this.pattern = delegate.getPattern();
        this.regex = Pattern.compile(pattern);
        this.type = RoutingRuleTypeEnum.of(delegate.getType());
        this.repositories = immuteRepositories(delegate.getRepositories());
    }

    private List<RoutingRuleRepository> immuteRepositories(List<MutableRoutingRuleRepository> source)
    {
        return source != null ? ImmutableList.copyOf(source.stream().map(RoutingRuleRepository::new).collect(
                Collectors.toList())) : Collections.emptyList();
    }

    public UUID getUuid()
    {
        return uuid;
    }

    @Override
    public String getStorageId()
    {
        return storageId;
    }

    @Override
    public String getRepositoryId()
    {
        return groupRepositoryId;
    }

    @Override
    public String getStorageIdAndRepositoryId()
    {
        StringJoiner stringJoiner = new StringJoiner(":");

        if (StringUtils.isNotBlank(storageId))
        {
            stringJoiner.add(storageId);
        }

        if (StringUtils.isNotBlank(groupRepositoryId))
        {
            stringJoiner.add(groupRepositoryId);
        }

        return stringJoiner.toString();
    }

    public String getGroupRepositoryId()
    {
        return groupRepositoryId;
    }

    public String getPattern()
    {
        return pattern;
    }

    public Pattern getRegex()
    {
        return regex;
    }

    public RoutingRuleTypeEnum getType()
    {
        return type;
    }

    public List<RoutingRuleRepository> getRepositories()
    {
        return repositories;
    }

    public boolean isDeny()
    {
        return type == RoutingRuleTypeEnum.DENY;
    }

    public boolean isAccept()
    {
        return type == RoutingRuleTypeEnum.ACCEPT;
    }
}
