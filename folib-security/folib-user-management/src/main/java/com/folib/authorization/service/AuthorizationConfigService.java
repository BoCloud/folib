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
package com.folib.authorization.service;

import com.folib.authorization.domain.AuthorizationConfig;
import com.folib.authorization.dto.RoleDto;
import com.folib.users.domain.Privileges;
import com.folib.authorization.dto.AuthorizationConfigDto;

import java.io.IOException;
import java.util.List;

/**
 * @author 
 * @author veadan
 */
public interface AuthorizationConfigService
{

    void setAuthorizationConfig(AuthorizationConfigDto config) throws IOException;

    AuthorizationConfigDto getDto();

    AuthorizationConfig get();
    AuthorizationConfig get(String username);

    void addRole(RoleDto role) throws IOException;

    boolean deleteRole(String roleName) throws IOException;

    void addPrivilegesToAnonymous(List<Privileges> privilegeList) throws IOException;

    /**
     * 处理角色信息
     *
     * @param roleInfo 角色信息
     */
    void handlerRole(String roleInfo);

    void clearPrivilegesAnonymous() throws IOException;
    AuthorizationConfig getRole(String roleId);
}
