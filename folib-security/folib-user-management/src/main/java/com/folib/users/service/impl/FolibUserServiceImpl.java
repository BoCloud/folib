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
package com.folib.users.service.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.folib.authorization.dto.Role;
import com.folib.constant.GlobalConstants;
import com.folib.converts.UserConvert;
import com.folib.domain.SecurityRole;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.dto.RepositoryPrivilegeDTO;
import com.folib.dto.UserDTO;
import com.folib.entity.FolibUser;
import com.folib.entity.RoleResourceRef;
import com.folib.mapper.FolibUserMapper;
import com.folib.users.dto.UserDto;
import com.folib.users.service.FolibUserService;
import com.folib.users.service.RoleResourceRefService;
import com.folib.users.service.UserGroupRefService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @Date: 2024/7/9 21:20
 * @Description:
 */
@Slf4j
@Component
@Transactional(rollbackFor=Exception.class)
public class FolibUserServiceImpl implements FolibUserService {

    @Inject
    private FolibUserMapper folibUserMapper;
    @Lazy
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Lazy
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    @Override
    public void deleteByUserName(String username) {
        folibUserMapper.deleteById(username);

        userGroupRefService.deleteByUserId(username);

        roleResourceRefService.deleteByentityId(username, GlobalConstants.ROLE_TYPE_USER);
    }

    @Override
    public UserDTO findByUserName(String username) {
        PageInfo<UserDTO> folibUsers = getUsers(UserDto.builder().id(username).build(), 1, 1);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        if (CollectionUtils.isNotEmpty(userEntities)) {
            return UserConvert.INSTANCE.UserEntityToUserDTO(userEntities.get(0));
        }
        return null;

    }

    @Override
    @Deprecated
    public List<UserDTO> findByUserNameResource(List<String> usernames, String storageId, String repositoryId, String path) {
        return folibUserMapper.queryUsersNameResource(usernames, storageId, repositoryId, path, null);
    }

    @Override
    public List<FolibUser> queryByIds(List<String> userIds) {
        return folibUserMapper.selectList(Wrappers.<FolibUser>lambdaQuery().in(FolibUser::getId, userIds));
    }

    @Override
    public UserEntity save(UserEntity userEntity) {
        FolibUser folibUser = UserConvert.INSTANCE.UserEntityToFolibUser(userEntity);
        FolibUser folibUserInfo = folibUserMapper.selectOne(Wrappers.<FolibUser>lambdaQuery().eq(FolibUser::getId, folibUser.getId()));
        if (Objects.equals(folibUserInfo, null)) {
            if (StringUtils.isBlank(folibUser.getSourceId()) || !"ldapUserDetailsService".equalsIgnoreCase(folibUser.getSourceId())) {
                folibUser.setSourceId("dataBaseUserDetailService");
            }
            folibUserMapper.insert(folibUser);
        }else {
            folibUser.setDeleted(GlobalConstants.NOT_DELETED);
            folibUserMapper.update(folibUser);
        }
        return userEntity;
    }

    @Override
    public Iterable<User> findAll() {
        long count = countUsers(UserDto.builder().build());
        if (count == 0L) {
            return null;
        }
        int limit = Math.toIntExact(count);
        PageInfo<UserDTO> folibUsers = getUsers(UserDto.builder().build(), 1, limit);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        return new ArrayList<>(userEntities);
    }

    @Override
    public Long countUsers(User user) {
        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            folibUser.setRoles(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()));
        }
        return folibUserMapper.countUserRole(folibUser);
    }

    @Override
    public PageInfo<User> findUsersPage(User user, int start, Integer limit) {
        PageInfo<UserDTO> folibUsers = getUsers(user, start, limit);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(folibUsers.getList());
        PageInfo<User> pageUser = new PageInfo<>(new ArrayList<>(userEntities));
        BeanUtils.copyProperties(folibUsers, pageUser, "list");
        return pageUser;
    }

    @Override
    public PageInfo<UserDTO> getUsers(User user, int start, Integer limit) {

        FolibUser folibUser = new FolibUser();
        BeanUtils.copyProperties(user, folibUser);
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            folibUser.setRoles(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toSet()));
        }
        PageHelper.startPage(start, limit);
        List<UserDTO> folibUsers = folibUserMapper.queryAllUserRoleByLimit(folibUser);
        PageInfo<UserDTO> pageUser = new PageInfo<>(folibUsers);
        //获取用户权限
        List<UserDTO> content = pageUser.getList();
        if (CollectionUtils.isNotEmpty(content)) {
            getUserAuthorities(content);
        }
        return pageUser;
    }

    /**
     * 获取用户权限
     * @param folibUsers
     */
    private void getUserAuthorities(List<UserDTO> folibUsers) {
        if (CollectionUtils.isNotEmpty(folibUsers)) {
            List<String> roleIds = folibUsers.stream().filter(userDTO -> CollectionUtils.isNotEmpty(userDTO.getRoles())).flatMap(userDTO -> userDTO.getRoles().stream()).distinct().collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(roleIds)) {
                //获取角色关联的api权限
                List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryResourcesByRoleIds(roleIds);
                Map<String, Set<String>> resourceMap = roleResourceRefs.stream()
                        .filter(r ->  GlobalConstants.RESOURCE_TYPE_API.equals(r.getResourceType()) && StringUtils.isNotEmpty(r.getApiAuthoritie())).collect(Collectors.groupingBy(RoleResourceRef::getRoleId, Collectors.mapping(RoleResourceRef::getApiAuthoritie, Collectors.toSet())));
                //获取角色关联的用户
                List<RoleResourceRef> refList = roleResourceRefService.queryPermissionsByRoleIds(roleIds);
                Map<String, String> entityMap = refList.stream()
                        .filter(r ->  GlobalConstants.RESOURCE_TYPE_API.equals(r.getResourceType()) && StringUtils.isNotEmpty(r.getApiAuthoritie())).collect(Collectors.toMap(RoleResourceRef::getRoleId, ref -> ref.getEntityId() + "_" + ref.getRefType()));

                folibUsers.forEach(userDTO -> {
                    Set<String> userApiAuthorities = new HashSet<>();
                    Set<String> roles = userDTO.getRoles();
                    if (CollectionUtils.isNotEmpty(roles)) {
                        roles.forEach(roleId -> {
                            Set<String> userAuthorities = Optional.ofNullable(resourceMap.get(roleId)).orElse(new HashSet<>());
                            userApiAuthorities.addAll(userAuthorities);
                        });
                    }
                    Set<String> userGroupIds = userDTO.getUserGroupIds();
                    if (CollectionUtils.isNotEmpty(userGroupIds)) {
                        Set<String> groupRoleIds = userGroupIds.stream().map(userGroupId -> entityMap.get(userGroupId + "_" + GlobalConstants.ROLE_TYPE_USER_GROUP))
                                .filter(StringUtils::isNotBlank)
                                .collect(Collectors.toSet());
                        Set<String> userGroupAuthorities = Optional.of(groupRoleIds).orElse(new HashSet<>()).stream().map(resourceMap::get).flatMap(Collection::stream).collect(Collectors.toSet());
                        if(CollectionUtils.isNotEmpty(userGroupAuthorities)) {
                            userApiAuthorities.addAll(userGroupAuthorities);
                        }
                    }
                    userDTO.setAuthorities(userApiAuthorities);
                });
            }

        }
    }

    @Override
    public boolean saveOrUpdateBatch(List<UserEntity> userEntitys) {
        List<FolibUser> folibUsers = UserConvert.INSTANCE.UserEntitysToFolibuiltyList(userEntitys);
        int i = folibUserMapper.insertOrUpdateBatch(folibUsers);
        return i == userEntitys.size();
    }

    @Override
    public Set<Role> queryRoles(String uuid) {
//        return folibUserMapper.queryRoles(uuid);
        return null;
    }

    @Override
    public void saveOrUpdate(List<FolibUser> users) {
        folibUserMapper.insertOrUpdateBatch(users);
    }

    @Override
    public PageInfo<FolibUser> paginQuery(FolibUser folibUser, PageRequest pageRequest) {
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<FolibUser> folibUsers = folibUserMapper.queryAllByLimit(folibUser);
        return new PageInfo<>(folibUsers);

    }

    @Override
    public List<User> queryUserRoleByRepositoryAndPrivilege(List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS) {
        List<UserDTO> users = folibUserMapper.queryUserRoleByRepositoryAndPrivilege(repositoryPrivilegeDTOS);
        List<UserEntity> userEntities = UserConvert.INSTANCE.UserDTOsToUserList(users);
        return new ArrayList<>(userEntities);
    }

}
