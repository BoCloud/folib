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
package com.folib.users.service;

import com.folib.domain.PageResultResponse;
import com.folib.domain.User;
import com.folib.users.domain.Users;
import org.jose4j.lang.JoseException;

import java.util.List;

/**
 * @author
 * @author veadan
 */
public interface UserService {

    User findByUsername(String username);

    /**
     * Generates another one 'Security Token' for specific user.<br>
     * Token will be based on 'username' with 'securityTokenKey' used as clam.
     *
     * @param username user ID
     * @return encrypted token
     * @throws JoseException
     */
    String generateSecurityToken(String username)
            throws JoseException;

    /**
     * Generates another one 'Security Token' for specific user.<br>
     * Token will be based on 'username' with 'securityTokenKey' used as clam.
     *
     * @param username      user ID
     * @param expireSeconds expire Seconds
     * @return encrypted token
     * @throws JoseException
     */
    String generateSecurityToken(String username, Integer expireSeconds)
            throws JoseException;

    /**
     * This method is mainly necessary for the UI - for users to be able to update their own account data
     * (i.e. change password or securityToken)
     *
     * @param userToUpdate
     */
    void updateAccountDetailsByUsername(User userToUpdate);

    Users getUsers();

    PageResultResponse<User> queryUser(User user, Integer page, Integer limit);

    void revokeEveryone(String roleToRevoke);

    User save(User user);

    User saveOverrideRole(User user);

    void deleteByUsername(String username);

    /**
     * 按角色查找用户
     *
     * @param rolesList 角色列表
     * @return 用户列表
     */
    List<User> findUserByRoles(List<String> rolesList);


}
