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
package com.folib.job.cron.jobs;

import com.folib.job.cron.domain.CronTaskConfigurationDto;

import java.util.Collection;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;

/**
 * @author veadan
 */
public class PerRepositoryDuplicationCheckStrategy
        implements CronJobDuplicationCheckStrategy
{

    private static final PerRepositoryDuplicationCheckStrategy DEFAULT = new PerRepositoryDuplicationCheckStrategy();

    private final String propertyStorageId;

    private final String propertyRepositoryId;

    private PerRepositoryDuplicationCheckStrategy()
    {
        this("storageId", "repositoryId");
    }

    public PerRepositoryDuplicationCheckStrategy(final String propertyStorageId,
                                                 final String propertyRepositoryId)
    {
        this.propertyStorageId = propertyStorageId;
        this.propertyRepositoryId = propertyRepositoryId;
    }

    public static PerRepositoryDuplicationCheckStrategy getDefault()
    {
        return DEFAULT;
    }

    @Override
    public boolean duplicates(final CronTaskConfigurationDto candidate,
                              final Collection<CronTaskConfigurationDto> existing)
    {
        if (CollectionUtils.isEmpty(existing))
        {
            return false;
        }
        return existing.stream().filter(e -> duplicates(candidate, e)).findFirst().isPresent();
    }

    private boolean duplicates(final CronTaskConfigurationDto first,
                               final CronTaskConfigurationDto second)
    {
        if (first == null || second == null)
        {
            return false;
        }

        final String firstJobClass = first.getJobClass();
        final String secondJobClass = second.getJobClass();

        if (!Objects.equals(firstJobClass, secondJobClass))
        {
            return false;
        }

        if (Objects.equals(first.getUuid(), second.getUuid()))
        {
            return false;
        }

        final String firstStorageId = first.getProperty(propertyStorageId);
        final String firstRepositoryId = first.getProperty(propertyRepositoryId);

        final String secondStorageId = second.getProperty(propertyStorageId);
        final String secondRepositoryId = second.getProperty(propertyRepositoryId);

        return Objects.equals(firstStorageId, secondStorageId) &&
               Objects.equals(firstRepositoryId, secondRepositoryId);
    }
}
