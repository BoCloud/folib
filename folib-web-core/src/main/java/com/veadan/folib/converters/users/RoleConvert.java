package com.veadan.folib.converters.users;

import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.forms.users.auth.RoleForm;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserGroupConvert
 */
@Mapper
public interface RoleConvert {

    RoleConvert INSTANCE = Mappers.getMapper(RoleConvert.class);

    @Mappings({})
    RoleDTO formToDto(RoleForm roleForm);

}
