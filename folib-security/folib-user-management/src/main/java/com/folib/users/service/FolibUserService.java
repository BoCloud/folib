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

import com.github.pagehelper.PageInfo;
import com.folib.authorization.dto.Role;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.dto.RepositoryPrivilegeDTO;
import com.folib.dto.UserDTO;
import com.folib.entity.FolibUser;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Set;

/**
  * @Description: 用户服务
  * @auther: fengmaogen
  * @CreateDate: 2024/7/9 21:16
  */
public interface FolibUserService {


    void deleteByUserName(String username);

    UserDTO findByUserName(String username);

    UserEntity save(UserEntity userEntity);

    Iterable<User> findAll();

    Long countUsers(User user);

    PageInfo<User> findUsersPage(User user, int start, Integer limit);

    boolean saveOrUpdateBatch(List<UserEntity> userEntitys);

    Set<Role> queryRoles(String uuid);

    void saveOrUpdate(List<FolibUser> users);

    PageInfo<FolibUser> paginQuery(FolibUser build, PageRequest pageRequest);

    List<User> queryUserRoleByRepositoryAndPrivilege(List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS);

    List<UserDTO> findByUserNameResource(List<String> usernames, String storageId, String repositoryId, String path);

    List<FolibUser> queryByIds(List<String> userIds);

    PageInfo<UserDTO> getUsers(User user, int start, Integer limit);
}
