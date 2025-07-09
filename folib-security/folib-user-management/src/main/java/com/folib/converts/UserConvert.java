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
package com.folib.converts;

import com.folib.domain.*;
import com.folib.dto.UserDTO;
import com.folib.entity.FolibUser;
import com.folib.users.dto.UserPermissionDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;
import org.mapstruct.factory.Mappers;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toSet;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserConvert
 */
@Mapper
public interface UserConvert {

    UserConvert INSTANCE = Mappers.getMapper(UserConvert.class);
    @Mappings({
           /* @Mapping(source = "userId", target = "userId"),
            @Mapping(source = "privileges", target = "privileges"),
            @Mapping(source = "roleIds", target = "roleIds")*/
    })
    UserPermissionDTO UserPermissionFormToUserPermissionDTO(UserPermissionForm userPermissionForm);

    @Mappings({@Mapping(target = "roles", ignore = true)})
    UserDTO FolibUserToUserDTO(FolibUser folibUser);
    @Mappings({})
    List<User> FolibUsersToUserList(List<FolibUser> folibUsers);
    @Mappings({@Mapping(source = "roles", target = "roles")})
    List<User> UserDTOsToUsers(List<UserDTO> usrerDTOList);

    @Mappings({})
    List<User> UserEntitysToUserList(List<UserEntity> folibUserList);

    @Mappings({@Mapping(source = "uuid", target = "username")})
    UserEntity UserToUserEntity(User user);

    @Mappings({@Mapping(source = "uuid", target = "username")})
    List<UserEntity> UserListToUserEntityList(List<User> users);

    @Mappings({@Mapping(source = "id", target = "uuid"),
            @Mapping(source = "updateTime", target = "lastUpdated")})
    UserEntity FolibUserUserEntity(FolibUser folibUser);

    @Mappings({})
    List<FolibUser> UserEntitysToFolibuiltyList(List<UserEntity> folibUserList);

    @Mappings({@Mapping(source = "folibUser.uuid", target = "id"),
            @Mapping(source = "lastUpdated", target = "updateTime"),
            @Mapping(target = "deleted", expression = "java(folibUser.isEnabled()?\"0\":\"1\")"),
            @Mapping(target = "roles", ignore = true)})
    FolibUser UserEntityToFolibUser(UserEntity folibUser);

    @Mappings({@Mapping(source = "roles", target = "roles"),
            @Mapping(source = "username", target = "username")})
    List<UserEntity> UserDTOsToUserList(List<UserDTO> usrerDTOList);

    @Mappings({@Mapping(source = "roles", target = "roles"),
            @Mapping(source = "updateTime", target = "lastUpdated"),
            @Mapping(source = "username", target = "username"),
            @Mapping(source = "username", target = "uuid")
    })
    UserEntity UserDTOToUserEntity(UserDTO usrerDTO);

    @Mappings({/*@Mapping(target = "roles", expression = "java(mapRolesToStrings(userEntity.getRoles()))"),*/
            @Mapping(target = "roles",qualifiedByName = "mapRolesToStrings"),
            @Mapping(target = "userGroups", expression = "java(userEntity.getUserGroups() != null && !userEntity.getUserGroups().isEmpty() ? String.join(\",\", userEntity.getUserGroups()) : \"\")"),
            @Mapping(target = "userGroupIds", expression = "java(userEntity.getUserGroupIds() != null && !userEntity.getUserGroupIds().isEmpty() ? String.join(\",\", userEntity.getUserGroupIds()) : \"\")")})
    UserDTO UserEntityToUserDTO(UserEntity userEntity);

    @Mappings({@Mapping(source = "username", target = "id")})
    FolibUser UserDTOToUser(UserDTO usrerDTO);
    @Named("mapRolesToStrings")
    default String mapRolesToStrings(Set<SecurityRole> roles) {
        if (roles == null) {
            return null;
        }
        return String.join(",", roles.stream()
                .map(SecurityRole::getRoleName)
                .collect(Collectors.toSet()));
    }

    // 自定义映射方法
    default Set<SecurityRole> map(Set<String> roles) {
        if (roles == null) {
            return new HashSet<>();
        }
        return roles.stream()
                .map(SecurityRoleEntity::new) // 将角色名称转换为 SecurityRole 对象
                .collect(toSet());
    }
    // 自定义方法将 Date 转换为 LocalDateTime
    default LocalDateTime map(Date updateTime) {
        return updateTime != null ? updateTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime(): null;
    }
    default Date map(LocalDateTime lastUpdated) {
        return lastUpdated != null ? Date.from(lastUpdated.atZone(ZoneId.systemDefault()).toInstant()) : null;
    }


}
