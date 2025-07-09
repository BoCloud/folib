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
package com.folib.users.userdetails;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.ConfigurationUtils;
import com.folib.users.domain.Privileges;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * @author veadan
 * @date 2025/4/16
 **/
public class UserPrivileges {

    public static boolean handlerRestrictedRepository(String serverName, Collection<Privileges> grantedAuthorities, String storageId, String repositoryId) {
        if (StringUtils.isBlank(serverName)) {
            return false;
        }
        List<String> restrictedSourceList = getRestrictedSource();
        if (CollectionUtils.isEmpty(restrictedSourceList)) {
            return false;
        }
        if (!restrictedSourceList.contains(serverName)) {
            return false;
        }
        List<String> restrictedRepositoryList = getRestrictedRepository();
        if (CollectionUtils.isEmpty(restrictedRepositoryList)) {
            return false;
        }
        if (!restrictedRepositoryList.contains(ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId))) {
            grantedAuthorities.removeAll(Privileges.restricted());
            return true;
        }
        return false;
    }

    public static List<String> getRestrictedSource() {
        List<String> restrictedSourceList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_SOURCE";
        String restrictedSource = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedSource)) {
            restrictedSourceList = Arrays.asList(restrictedSource.split(","));
        }
        return restrictedSourceList;
    }

    public static List<String> getRestrictedRepository() {
        List<String> restrictedRepositoryList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_REPOSITORY";
        String restrictedRepository = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedRepository)) {
            restrictedRepositoryList = Arrays.asList(restrictedRepository.split(","));
        }
        return restrictedRepositoryList;
    }
}
