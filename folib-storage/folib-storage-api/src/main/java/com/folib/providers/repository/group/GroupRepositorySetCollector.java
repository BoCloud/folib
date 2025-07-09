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
package com.folib.providers.repository.group;

import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Slf4j
@Component
public class GroupRepositorySetCollector
{

    @Inject
    private ConfigurationManager configurationManager;

    public Set<Repository> collect(Repository groupRepository)
    {
        return collect(groupRepository, false);
    }

    public Set<Repository> collect(Repository groupRepository,
                                   boolean traverse)
    {
        log.info("GroupRepository {}", groupRepository.getId());
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(groupRepository, storageAndRepositoryIdList);
        Set<Repository> result = storageAndRepositoryIdList.stream()
                                                .map(groupRepoId -> getRepository(groupRepository.getStorage(),
                                                                                  groupRepoId))
                                                .collect(Collectors.toCollection(LinkedHashSet::new));

        if (!traverse)
        {
            return result;
        }

        Set<Repository> traverseResult = new LinkedHashSet<>();
        for (Iterator<Repository> i = result.iterator(); i.hasNext(); )
        {
            Repository r = i.next();
            if (CollectionUtils.isEmpty(r.getGroupRepositories()))
            {
                traverseResult.add(r);
                continue;
            }

            i.remove();
            traverseResult.addAll(collect(r, true));
        }

        return traverseResult;
    }

    private Repository getRepository(Storage storage,
                                     String id)
    {
        String sId = ConfigurationUtils.getStorageId(storage.getId(), id);
        String rId = ConfigurationUtils.getRepositoryId(id);

        return configurationManager.getConfiguration().getStorage(sId).getRepository(rId);
    }

}
