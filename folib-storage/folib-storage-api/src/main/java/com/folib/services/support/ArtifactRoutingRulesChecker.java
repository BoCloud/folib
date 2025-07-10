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
package com.folib.services.support;

import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import com.folib.storage.routing.RepositoryIdentifiable;
import com.folib.storage.routing.RoutingRule;
import com.folib.storage.routing.RoutingRules;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @see <a href="https://folib.github.io/user-guide/artifact-routing-rules.html">Artifact Routing Rules</a>
 */
@Component
public class ArtifactRoutingRulesChecker
{

    @Inject
    private ConfigurationManager configurationManager;

    public boolean isDenied(Repository groupRepository,
                            RepositoryPath repositoryPath)
            throws IOException
    {
        final RoutingRules routingRules = configurationManager.getConfiguration().getRoutingRules();
        final boolean hasDenyRules = hasCandidates(groupRepository, repositoryPath, routingRules.getDenied());
        final boolean hasAcceptRules = hasCandidates(groupRepository, repositoryPath, routingRules.getAccepted());

        return hasDenyRules && !hasAcceptRules;
    }

    private boolean hasCandidates(Repository groupRepository,
                                  RepositoryPath repositoryPath,
                                  List<RoutingRule> routingRules)
            throws IOException
    {

        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        Repository subRepository = repositoryPath.getRepository();

        return routingRules.stream()
                           .anyMatch(rule -> {
                               boolean result = false;

                               if ((isMatch(rule, groupRepository))
                                   && rule.getRegex().matcher(artifactPath).matches())
                               {
                                   // an empty collection means the rule is applied to **all** repositories in the group.
                                   if (rule.getRepositories().size() == 0)
                                   {
                                       result = true;
                                   }
                                   else
                                   {
                                       result = rule.getRepositories()
                                                    .stream()
                                                    .anyMatch(r -> isMatch(r, subRepository));
                                   }
                               }

                               return result;
                           });

    }

    private boolean isMatch(RepositoryIdentifiable rule,
                            Repository repository)
    {
        boolean result = false;

        // exact match == storageId:repositoryId
        if (equalsIgnoreCase(rule.getStorageIdAndRepositoryId(), repository.getStorageIdAndRepositoryId()))
        {
            result = true;
        }
        // wildcard == *:*
        else if (equalsIgnoreCase(rule.getStorageIdAndRepositoryId(), StringUtils.EMPTY))
        {
            result = true;
        }
        // wildcard == storageId:*
        else if (equalsIgnoreCase(rule.getRepositoryId(), StringUtils.EMPTY) &&
                 equalsIgnoreCase(rule.getStorageId(), repository.getStorage().getId()))
        {
            result = true;
        }
        // wildcard == *:repositoryId
        else if (equalsIgnoreCase(rule.getStorageId(), StringUtils.EMPTY) &&
                 equalsIgnoreCase(rule.getRepositoryId(), repository.getId()))
        {
            result = true;
        }

        return result;
    }

    private boolean equalsIgnoreCase(final String a,
                                     final String b)
    {
        return StringUtils.trimToEmpty(a).equalsIgnoreCase(StringUtils.trimToEmpty(b));
    }
}
