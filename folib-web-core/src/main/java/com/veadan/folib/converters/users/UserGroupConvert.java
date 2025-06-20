package com.veadan.folib.converters.users;

import com.veadan.folib.controllers.users.support.UserGroupOutput;
import com.veadan.folib.dto.UserGroupListDTO;
import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.dto.users.UserGroupDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserGroupConvert
 */
@Mapper
public interface UserGroupConvert {

    UserGroupConvert INSTANCE = Mappers.getMapper(UserGroupConvert.class);

    @Mappings({})
    List<UserGroup> UserGroupDTOToEntities(List<UserGroupListDTO> userGroups);
    @Mappings({})
    List<UserGroupOutput> UserGroupsToOutput(List<UserGroup> userGroups);

    @Mappings({})
    UserGroup UserGroupToUserGroupForm(UserGroupDto userGroupForm);
    @Mappings({})
    UserGroupOutput UserGroupToOutput(UserGroup userGroup);
}
