package com.veadan.folib.converts;

import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.domain.SecurityRoleEntity;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.dto.UserDTO;
import com.veadan.folib.entity.FolibUser;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserConvert
 */
@Mapper
public interface UserConvert {

    UserConvert INSTANCE = Mappers.getMapper(UserConvert.class);

    @Mappings({})
    UserDTO FolibUserToUserDTO(FolibUser folibUser);
    @Mappings({})
    List<User> FolibUsersToUserList(List<FolibUser> folibUsers);
    @Mappings({@Mapping(source = "roles", target = "roles")})
    List<User> UserDTOsToUsers(List<UserDTO> usrerDTOList);

    @Mappings({})
    List<User> UserEntitysToUserList(List<UserEntity> folibUserList);

    @Mappings({})
    List<UserEntity> UserListToUserEntityList(List<User> users);
    @Mappings({@Mapping(source = "id", target = "uuid"),
            @Mapping(source = "updateTime", target = "lastUpdated")})
    UserEntity FolibUserUserEntity(FolibUser folibUser);

    @Mappings({})
    List<FolibUser> UserEntitysToFolibuiltyList(List<UserEntity> folibUserList);

    @Mappings({@Mapping(source = "folibUser.username", target = "id"),
            @Mapping(source = "lastUpdated", target = "updateTime"),
            @Mapping(target = "deleted", expression = "java(folibUser.isEnabled()?\"0\":\"1\")")})
    FolibUser UserEntityToFolibUser(UserEntity folibUser);

    @Mappings({@Mapping(source = "roles", target = "roles")})
    List<UserEntity> UserDTOsToUserList(List<UserDTO> usrerDTOList);
    @Mappings({@Mapping(source = "roles", target = "roles")})
    UserEntity UserDTOToUserEntity(UserDTO usrerDTO);

    @Mappings({@Mapping(source = "roles", target = "roles")})
    FolibUser UserDTOToUser(UserDTO usrerDTO);
    // 自定义映射方法
    default Set<SecurityRole> map(Set<String> roles) {
        return roles.stream()
                .map(SecurityRoleEntity::new) // 将角色名称转换为 SecurityRole 对象
                .collect(Collectors.toSet());
    }
    // 自定义方法将 Date 转换为 LocalDateTime
    default LocalDateTime map(Date updateTime) {
        return updateTime != null ? updateTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime(): null;
    }
    default Date map(LocalDateTime lastUpdated) {
        return lastUpdated != null ? Date.from(lastUpdated.atZone(ZoneId.systemDefault()).toInstant()) : null;
    }

}
