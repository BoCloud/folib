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
package com.folib.services.impl;

import com.folib.services.ConfigurationManagementService;
import com.folib.services.StorageManagementService;
import com.folib.services.YamlDataService;
import com.folib.users.service.FolibRoleService;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author veadan
 * @Date: 2024/8/16 11:26
 * @Description:
 */
@Slf4j
@Service
public class YamlDataServiceImpl implements YamlDataService {

    @Autowired
    private StorageManagementService storageManagementService;
    @Autowired
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;
    @Autowired
    private FolibRoleService folibRoleService;

    @Override
    public void syncYamlData() {
        //同步角色
        folibRoleService.syncYamlAuthorizationConfig();
        //同步存储空间用户
        storageManagementService.syncYamlStorageUsers(configurationManagementService.getConfiguration().getStorages().values());
        //同步用户
        ((RelationalDatabaseUserService) userService).syncUser();
;
    }
}
