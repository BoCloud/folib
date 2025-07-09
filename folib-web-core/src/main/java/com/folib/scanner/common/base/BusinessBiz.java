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


package com.folib.scanner.common.base;

import com.folib.configuration.MutableConfiguration;
import com.folib.security.authentication.JwtTokenFetcher;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.StorageManagementService;
import com.folib.storage.StorageDto;
import com.folib.users.security.SecurityTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import java.util.*;

/**
 * 基础业务类
 *
 * @author Veadan
 * @version 2018/1/13.
 */
public abstract class BusinessBiz  implements JwtTokenFetcher {

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;
    @Inject
    private StorageManagementService storageManagementService;


    /**
     * 获取登录用户名
     *
     * @return 登录用户名
     */
    protected String loginUsername() {
        HttpServletRequest request = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getRequest();
        Optional<String> token = getToken(request);
        String username = securityTokenProvider.getSubject(token.get());
        return username;
    }

    /**
     * 获取配置信息
     *
     * @return 配置信息
     */
    protected MutableConfiguration mutableConfiguration() {
        return configurationManagementService.getMutableConfigurationClone();
    }

    /**
     * 获取没有权限访问的存储空间id列表
     *
     * @return 没有权限访问的存储空间id列表
     */
    public List<String> withoutPermissionStorageIdList() {
        List<String> allStorageIdList = new ArrayList<String>(mutableConfiguration().getStorages().keySet());
        List<String> storageIdList = havePermissionStorageIdList();
        allStorageIdList.removeAll(storageIdList);
        return allStorageIdList;
    }

    /**
     * 获取有权限访问的存储空间id列表
     *
     * @return 有权限访问的存储空间id列表
     */
    public List<String> havePermissionStorageIdList() {
        List<String> storageIdList = Lists.newArrayList();
        String username = loginUsername();
        String admin = "admin";
        if (admin.equals(username)) {
            storageIdList = new ArrayList<>(mutableConfiguration().getStorages().keySet());
            return storageIdList;
        }
        for (Map.Entry<String, StorageDto> entry : mutableConfiguration().getStorages().entrySet()) {
            //查询数据库中存储空间绑定的用户
            String storageId = entry.getKey();
            Map<String, Set<String>> storageUser = storageManagementService.getStorageUser(Collections.singleton(storageId));
            Set<String> userSet = storageUser.get(storageId);
            if (CollectionUtils.isNotEmpty(userSet)) {
                if (userSet.contains(username)) {
                    storageIdList.add(storageId);
                }
            }
        }
        return storageIdList;
    }

    /**
     * 获取基础查询参数
     *
     * @return 基础查询参数
     */
    public BaseQuery getBaseQuery() {
        List<String> storageIdList = havePermissionStorageIdList();
        List<String> notInStorageIdList = withoutPermissionStorageIdList();
        return BaseQuery.builder().notInStorageIdList(notInStorageIdList).storageIdList(storageIdList).build();
    }
}
