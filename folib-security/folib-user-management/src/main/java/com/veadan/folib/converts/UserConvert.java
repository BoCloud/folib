package com.veadan.folib.converts;

import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.entity.FolibUser;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserConvert
 */
@Mapper
public interface UserConvert {

    UserConvert INSTANCE = Mappers.getMapper(UserConvert.class);

    @Mappings({})
    List<User> UserEntitysToUserList(List<UserEntity> folibUserList);

    @Mappings({})
    List<UserEntity> UserListToUserEntityList(List<User> users);

    @Mappings({@Mapping(source = "uuid", target = "username")})
    List<FolibUser> UserEntitysToFolibuiltyList(List<UserEntity> folibUserList);
}
